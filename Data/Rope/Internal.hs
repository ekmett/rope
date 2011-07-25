{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, UndecidableInstances, TypeOperators, DeriveDataTypeable #-}
module Data.Rope.Internal
    ( Rope(..)
    -- * Construction
    , Packable(..)
    , empty                 -- :: Rope
    , fromChunks            -- :: [ByteString] -> Rope
    , fromByteString        -- :: ByteString -> Rope
    , fromLazyByteString    -- :: L.ByteString -> Rope
    , fromString            -- :: String -> Rope
    , fromWords             -- :: [Word8] -> Rope
    , fromChar              -- :: Char -> Rope
    , fromWord8             -- :: Word8 -> Rope
    -- * Analysis
    , length                -- :: Rope -> Int
    , null                  -- :: Rope -> Bool
    -- * Deconstruction
    , toChunks              -- :: Rope -> [ByteString]
    , toString              -- :: Rope -> String
    , toLazyByteString      -- :: Rope -> L.ByteString
    -- * Cutting 
    , splitAt
    , take
    , drop
    -- * Unpacking
    , Unpackable(..)
    , Breakable(..)
    -- Utility 
    , w2c
    , findIndexOrEnd        -- :: (Word8 -> Bool) -> ByteString -> Int
    ) where


import Prelude hiding (head, last, length, foldl, null, length, splitAt, take, drop, break, span)
import qualified Prelude

import Control.Applicative hiding (empty)

import Data.Data (Data(..), DataType, Constr, Fixity(..), mkConstr, mkDataType, constrIndex)
import Data.Typeable (Typeable(..))

import Data.FingerTree (ViewL(..),ViewR(..),viewl,viewr,(<|),(|>), Measured(..), (><))
import qualified Data.FingerTree as F (empty, split, null, singleton)

import qualified Data.Foldable as F

import Data.Monoid

import Data.Rope.Body

import Data.Word (Word8)

import GHC.Base (unsafeChr)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek)

import qualified Data.ByteString as S (null, splitAt, take, drop, length, singleton, unpack, last)
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import qualified Data.ByteString.Unsafe as S (unsafeTail, unsafeHead)
import qualified Data.ByteString.UTF8 as U (fromString)
import qualified Data.ByteString.Lazy as L (ByteString, pack, fromChunks, toChunks, elemIndex)
import qualified Data.ByteString.Lazy.UTF8 as LU (fromString, toString)

import Codec.Binary.UTF8.Generic (UTF8Bytes)
import qualified Codec.Binary.UTF8.Generic as UTF8Bytes

-- a Buffer is a fingertree of non-empty chunks
newtype Rope = Rope { body :: Body } 
    deriving (Show, Typeable)

instance Monoid Rope where
    mempty = empty
    Rope t `mappend` Rope t' = Rope (t >< t')

instance Eq Rope where
    a == b = measure (body a) == measure (body b) 
          && toLazyByteString a == toLazyByteString b 

instance Ord Rope where
    a `compare` b = toLazyByteString a `compare` toLazyByteString b

instance Measured Offset Rope where
    measure = measure . body

-- Minimal definition: 'unit' or 'snocBody
class Packable c where
    pack :: c -> Rope
    snoc :: Rope -> c -> Rope
    cons :: c -> Rope -> Rope

    pack = snoc mempty 
    snoc m = mappend m . pack
    cons = mappend . pack

empty :: Rope
empty = Rope F.empty
{-# INLINE empty #-}

fromChunks :: [ByteString] -> Rope
fromChunks = foldr (\l (Rope t) -> Rope (l `consBody` t)) mempty
{-# INLINE fromChunks #-}

toChunks :: Rope -> [ByteString]
toChunks r = unchunk <$> F.toList (body r)
{-# INLINE toChunks #-}

toLazyByteString :: Rope -> L.ByteString
toLazyByteString = L.fromChunks . toChunks
{-# INLINE toLazyByteString #-}

toString :: Rope -> String
toString = unpack
{-# INLINE toString #-}

length :: Rope -> Int
length = measureBody . body
{-# INLINE length #-}

null :: Rope -> Bool
null = F.null . body
{-# INLINE null #-}

fromByteString :: ByteString -> Rope
fromByteString b | S.null b = mempty 
                 | otherwise = Rope (F.singleton (Chunk b))
{-# INLINE fromByteString #-}

-- NB this requires a strict bytestring reducer, but a lazy bytestring
fromLazyByteString :: L.ByteString -> Rope
fromLazyByteString = foldr (\l (Rope t) -> Rope (Chunk l <| t)) mempty . L.toChunks
{-# INLINE fromLazyByteString #-}

-- utf8 encode chunks of the string
fromString :: String -> Rope
fromString = fromLazyByteString . LU.fromString
{-# INLINE fromString #-}

fromWords :: [Word8] -> Rope
fromWords = fromLazyByteString . L.pack
{-# INLINE fromWords #-}

fromChar :: Char -> Rope
fromChar c = Rope (F.singleton (Chunk (U.fromString [c])))
{-# INLINE fromChar #-}

fromWord8 :: Word8 -> Rope
fromWord8 b = Rope (F.singleton (Chunk (S.singleton b)))
{-# INLINE fromWord8 #-}

cons8 :: Word8 -> Rope -> Rope
cons8 a (Rope t) = case viewl t of
    Chunk c :< cs | S.length c < 16 -> Rope (Chunk (mappend b c) <| cs)
    _                               -> Rope (Chunk b <| t)
    where b = S.singleton a
{-# INLINE cons8 #-}

instance Data Rope where
    gfoldl f z r = case uncons8 r of
        Nothing -> z empty
        Just (x,xs) -> z cons8 `f` x `f` xs 
     
    gunfold k z c = case constrIndex c of
       1 -> z empty
       2 -> k (k (z cons8))
       _ -> error "gunfoldl"
    
    toConstr xs
       | null xs = emptyConstr
       | otherwise = consConstr

    dataTypeOf _ = ropeDataType

emptyConstr, consConstr :: Constr
emptyConstr = mkConstr ropeDataType "empty" [] Prefix
consConstr = mkConstr ropeDataType "`cons`" [] Infix

ropeDataType :: DataType
ropeDataType = mkDataType "Data.Rope.Internal.Rope" [emptyConstr, consConstr]

splitAt :: Int -> Rope -> (Rope,Rope)
splitAt n (Rope f)
        | n <= 0 = (mempty, Rope f)
        | n >= measureBody f = (Rope f, mempty)
        | otherwise = (Rope (x `snocBody` y'), Rope (y'' `consBody` z))
        where
            (x,yz) = F.split (> Offset n) f
            Chunk y :< z = viewl yz
            (y', y'') = S.splitAt (n - measureBody x) y

take :: Int -> Rope -> Rope
take n = fst . splitAt n
{-# INLINE take #-}

drop :: Int -> Rope -> Rope
drop n = snd . splitAt n
{-# INLINE drop #-}

class Breakable a where
    break :: (a -> Bool) -> Rope -> (Rope, Rope)
    span :: (a -> Bool) -> Rope -> (Rope, Rope)
    takeWhile :: (a -> Bool) -> Rope -> Rope
    dropWhile :: (a -> Bool) -> Rope -> Rope

    span f = break (not . f)
    takeWhile f = fst . span f
    dropWhile f = snd . span f

break8 :: (Word8 -> Bool) -> Rope -> (Rope, Rope)
break8 f r = (Rope t', Rope t'')
    where 
        (t',t'') = break' (body r)
        break' ccs = case viewl ccs of
           EmptyL -> (F.empty, F.empty)
           Chunk c :< cs -> case findIndexOrEnd f c of 
                0              -> (F.empty, ccs)
                n | n < S.length c -> (F.singleton (Chunk (S.take n c)), Chunk (S.drop n c) <| cs)
                  | otherwise      -> let (cs', cs'') = break' cs
                                      in (Chunk c <| cs', cs'')
{-# INLINE break8 #-}

instance Breakable Word8 where
    break = break8

findIndexOrEnd :: (Word8 -> Bool) -> ByteString -> Int
findIndexOrEnd k (PS x s l) = inlinePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go ptr n | ptr `seq` n `seq` False = undefined
             | n >= l    = return l
             | otherwise = do w <- peek ptr
                              if k w
                                then return n
                                else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}

uncons8 :: Rope -> Maybe (Word8, Rope)
uncons8 r = case viewl (body r) of
    Chunk c :< cs -> Just (S.unsafeHead c, Rope (S.unsafeTail c `consBody` cs))
    _ -> Nothing
{-# INLINE uncons8 #-}

unsnoc8 :: Rope -> Maybe (Rope, Word8)
unsnoc8 r = case viewr (body r) of
    cs :> Chunk c -> Just (Rope (cs `snocBody` S.unsafeTail c), S.unsafeHead c)
    _ -> Nothing
{-# INLINE unsnoc8 #-}

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

instance Packable Char where
    pack = fromChar
    cons a (Rope t) = case viewl t of
        Chunk c :< cs | S.length c < 16 -> Rope (Chunk (mappend b c) <| cs)
        _ -> Rope (Chunk b <| t)
        where b = U.fromString [a]
    snoc (Rope t) a = case viewr t of
        cs :> Chunk c | S.length c < 16 -> Rope (cs |> Chunk (mappend c b))
        _ -> Rope (t |> Chunk b)
        where b = U.fromString [a]

instance Packable Word8 where
    pack = fromWord8
    cons = cons8 
    snoc (Rope t) a = case viewr t of
        cs :> Chunk c | S.length c < 16 -> Rope (cs |> Chunk (mappend c b))
        _ -> Rope (t |> Chunk b)
        where b = S.singleton a

instance Packable Rope where
    pack = id

instance Packable String where
    pack = fromString

instance Packable [Word8] where
    pack = fromWords

instance Packable ByteString where
    pack = fromByteString

instance Packable L.ByteString where
    pack = fromLazyByteString

instance Packable Chunk where
    pack = fromByteString . unchunk 

instance UTF8Bytes Rope Int where
    bsplit = splitAt 
    bdrop = drop 
    buncons f = case viewl (body f) of
        Chunk c :< cs -> Just (S.unsafeHead c, Rope (S.unsafeTail c `consBody ` cs))
        EmptyL -> Nothing
    tail (Rope f) = case viewl f of
        Chunk c :< cs -> Rope (S.unsafeTail c `consBody`cs)
        EmptyL -> errorEmptyList "tail"
    elemIndex b = fmap fromIntegral . L.elemIndex b . L.fromChunks . map unchunk . F.toList . body
    pack = Rope . foldr (\l r -> Chunk l <| r) F.empty . L.toChunks . L.pack
    empty = Rope F.empty
    null = F.null . body


class Unpackable a where
    unpack :: Rope -> [a]

    head :: Rope -> a
    head = Prelude.head . unpack

    last :: Rope -> a

    uncons :: Rope -> Maybe (a, Rope)
    unsnoc :: Rope -> Maybe (Rope, a)

instance Unpackable Word8 where
    unpack = concatMap (S.unpack . unchunk) . F.toList . body
    head t = case viewl (body t) of
        Chunk a :< _ -> S.unsafeHead a
        EmptyL -> errorEmptyList "head"
    last t = case viewr (body t) of
        _ :> Chunk a -> S.last a
        EmptyR -> errorEmptyList "last"
    uncons = uncons8
    unsnoc = unsnoc8

instance Unpackable Char where
    unpack = LU.toString . toLazyByteString
    head = Prelude.head . unpack
    last = undefined -- TODO
    uncons r@(Rope t) = case UTF8Bytes.decode (Rope t) of 
        Nothing -> Nothing
        Just (a,n) -> Just (a, drop n r)
    unsnoc = undefined -- TODO

instance Unpackable ByteString where
    unpack = map unchunk . F.toList . body
    head r = case viewl (body r) of
        Chunk a :< _ -> a
        _ -> errorEmptyList "head"
    last r = case viewr (body r) of
        _ :> Chunk a -> a
        _ -> errorEmptyList "last" 
    uncons r = case viewl (body r) of
        Chunk a :< as -> Just (a, Rope as)
        EmptyL -> Nothing
    unsnoc r = case viewr (body r) of
        as :> Chunk a -> Just (Rope as, a)
        EmptyR -> Nothing

instance Unpackable Chunk where
    unpack = F.toList . body
    head r = case viewl (body r) of
        a :< _ -> a
        _ -> errorEmptyList "head"
    last r = case viewr (body r) of
        _ :> a -> a
        _ -> errorEmptyList "last"
    uncons r = case viewl (body r) of
        Chunk a :< as -> Just (Chunk a, Rope as)
        EmptyL -> Nothing
    unsnoc r = case viewr (body r) of
        as :> Chunk a -> Just (Rope as, Chunk a)
        EmptyR -> Nothing

errorEmptyList :: String -> a
errorEmptyList t = error $ "Data.Rope.Unpackable." ++ t ++ ": empty list"  
