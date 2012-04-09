{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module Data.Rope.Body
    ( Body
    , Offset(..)
    , Chunk(..)
    , measureBody
    , consBody
    , snocBody
    , packBody
    ) where

import Prelude hiding (null, length)
import Data.FingerTree (FingerTree,(<|),(|>),Measured,measure,empty, singleton)
import Data.Data
import Data.Monoid
import Data.ByteString (ByteString, null, length)

newtype Offset = Offset { getOffset :: Int } deriving (Eq,Ord,Num,Show,Read,Enum,Data,Typeable)

instance Monoid Offset where
    mempty = 0
    mappend = (+)

newtype Chunk = Chunk { unchunk :: ByteString } deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Measured Offset Chunk where
    measure = Offset . length . unchunk

type Body = FingerTree Offset Chunk 

measureBody :: Measured Offset a => FingerTree Offset a -> Int
measureBody = getOffset . measure

consBody :: ByteString -> Body -> Body
b `consBody` t | null b = t
           | otherwise = Chunk b <| t

snocBody :: Body -> ByteString -> Body
t `snocBody` b | null b = t
           | otherwise = t |> Chunk b

packBody :: ByteString -> FingerTree Offset Chunk
packBody b | null b = empty
           | otherwise = singleton (Chunk b)
