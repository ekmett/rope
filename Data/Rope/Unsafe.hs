{-# LANGUAGE TypeOperators, EmptyDataDecls, 
             MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             TypeFamilies, IncoherentInstances, OverlappingInstances #-}
module Data.Rope.Unsafe
    ( -- * Annotations
      MonoidalAnn
    , PackableAnn
    , BreakableAnn 
    -- * Constructors
    , Branded
    , Ann
    , Unsafe

      -- * Unpacking 'Ropes'
    , null      -- :: (s `Branded` Rope) a -> Bool
    , head      -- :: Unpackable t => (s `Branded` Rope) a -> t
    , last      -- :: Unpackable t => (s `Branded` Rope) a -> t
    , unpack    -- :: Unpackable t => (s `Branded` Rope) a -> [t]

      -- * Building Annotated 'Rope'
    , empty     -- :: MonoidalAnn f => Ann f Unsafe 
    , append    -- :: MonoidalAnn f => Ann f a -> Ann f b -> Ann f Unsafe

    , Packable
    , pack
    , snoc
    , cons

      -- * Cutting An Annotated 'Rope'
    , splitAt   -- :: BreakableAnn f => Int -> Ann f a -> (Ann f Unsafe, Ann f Unsafe)
    , drop      -- :: BreakableAnn f => Int -> Ann f a -> Ann f Unsafe
    , take      -- :: BreakableAnn f => Int -> Ann f a -> Ann f Unsafe

    , break     -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (Ann f Unsafe, Ann f Unsafe)
    , span      -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (Ann f Unsafe, Ann f Unsafe)
    , takeWhile -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> Ann f Unsafe
    , dropWhile -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> Ann f Unsafe

    -- * Inspecting the ends of the 'Rope'
    , uncons    -- :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (t, Ann f Unsafe)
    , unsnoc    -- :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (Ann f Unsafe, t)

    ) where

import Prelude hiding (null, head, last, take, drop, span, break, splitAt, takeWhile, dropWhile)
import Data.Monoid

import qualified Data.Rope.Internal as Rope

import Data.Rope.Annotation (MonoidalAnn(..), PackableAnn(..), BreakableAnn(..))
import Data.Rope.Annotated (Ann)
import Data.Rope.Branded (Branded(..), null, head, last, unpack, Unsafe)

import Data.Rope.Internal (Packable, Breakable, Unpackable)

empty :: MonoidalAnn f => Ann f Unsafe
empty = Branded Rope.empty emptyAnn

append :: MonoidalAnn f => Ann f a -> Ann f b -> Ann f Unsafe
append (Branded r a) (Branded s b) = Branded (r `mappend` s) (appendAnn r a s b)

pack :: (PackableAnn f, Packable t) => t -> Ann f Unsafe 
pack t = Branded r (packAnn r) 
    where r = Rope.pack t

splitAt :: BreakableAnn f => Int -> Ann f a -> (Ann f Unsafe, Ann f Unsafe)
splitAt n (Branded r a) = (Branded r b, Branded r c) 
    where (b, c) = splitAtAnn n r a

drop :: BreakableAnn f => Int -> Ann f a -> Ann f Unsafe
drop n (Branded r a) = Branded r (dropAnn n r a)

take :: BreakableAnn f => Int -> Ann f a -> Ann f Unsafe
take n (Branded r a) = Branded r (takeAnn n r a)

snoc :: (PackableAnn f, Packable t) => Ann f a -> t -> Ann f Unsafe
snoc (Branded r a) t = Branded r' (snocAnn (Rope.length r' - Rope.length r) r' a)
    where r' = Rope.snoc r t 

cons :: (PackableAnn f, Packable t) => t -> Ann f a -> Ann f Unsafe
cons t (Branded r a) = Branded r' (consAnn (Rope.length r' - Rope.length r) r' a)
    where r' = Rope.cons t r

break :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (Ann f Unsafe, Ann f Unsafe)
break p (Branded r a) = (Branded x b, Branded y c) where
    (x,y) = Rope.break p r
    (b,c) = splitAtAnn (Rope.length x) r a

span :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (Ann f Unsafe, Ann f Unsafe)
span p (Branded r a) = (Branded x b, Branded y c) where
    (x,y) = Rope.span p r
    (b,c) = splitAtAnn (Rope.length x) r a

takeWhile :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> Ann f Unsafe
takeWhile p (Branded r a) = Branded x b where
    x = Rope.takeWhile p r
    b = takeAnn (Rope.length x) r a

dropWhile :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> Ann f Unsafe
dropWhile p (Branded r a) = Branded y c where
    y = Rope.dropWhile p r
    c = dropAnn (Rope.length r - Rope.length y) r a

uncons :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (t, Ann f Unsafe)
uncons (Branded r a) = case Rope.uncons r of
    Just (c,cs) -> Just (c, Branded cs (dropAnn (Rope.length r - Rope.length cs) r a))
    Nothing -> Nothing

unsnoc :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (Ann f Unsafe, t)
unsnoc (Branded r a) = case Rope.unsnoc r of
    Just (cs,c) -> Just (Branded cs (dropAnn (Rope.length cs) r a), c)
    Nothing -> Nothing

