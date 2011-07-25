{-# LANGUAGE TypeOperators, EmptyDataDecls, 
             MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             TypeFamilies, IncoherentInstances, OverlappingInstances #-}
module Data.Rope.Annotated.Unsafe
    ( -- * Annotated 'Rope's 
      U
    , MonoidA, ReducerA, BreakableA 

      -- * Unpacking 'Ropes'
    , null      -- :: A s a -> Bool
    , head      -- :: Unpackable t => A s a -> t
    , last      -- :: Unpackable t => A s a -> t
    , unpack    -- :: Unpackable t => A s a -> [t]

      -- * Building Annotated 'Rope'
    , empty     -- :: MonoidA f => U f 
    , append    -- :: MonoidA f => Ann a f -> Ann b f -> U f

    , unit      -- :: (ReducerA f, Reducer t Rope) => t -> U f
    , snoc      -- :: (ReducerA f, Reducer t Rope) => t -> Ann a f -> U f
    , cons      -- :: (ReducerA f, Reducer t Rope) => Ann a f -> t -> U f

      -- * Cutting An Annotated 'Rope'
    , splitAt   -- :: (BreakablaA f) => Int -> Ann a f -> (U f, U f)
    , drop      -- :: (BreakableA f) => Int -> Ann a f -> U f
    , take      -- :: (BreakablaA f) => Int -> Ann a f -> U f

    , break     -- :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> (U f, U f)
    , span      -- :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> (U f, U f)
    , takeWhile -- :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> U f
    , dropWhile -- :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> U f

    -- * Inspecting the ends of the 'Rope'
    , uncons    -- :: (BreakableA f, Unpackable t) => Ann a f -> Maybe (t, U f)
    , unsnoc    -- :: (BreakableA f, Unpackable t) => Ann a f -> Maybe (U f, t)

    , Unsafe
    ) where

import Prelude hiding (null, head, last, take, drop, span, break, splitAt, takeWhile, dropWhile)
import Data.Monoid

import qualified Data.Rope.Internal as Rope

import Data.Rope.Annotated.Internal (A(..), null, head, last, unpack)
import Data.Rope.Annotated (Ann)
import Data.Rope.Annotation

import Data.Rope.Util.Reducer (Reducer)
import qualified Data.Rope.Util.Reducer as Reducer

import Data.Rope.Internal (Rope(..),Breakable, Unpackable)

data Unsafe

type U f = A Unsafe (f Unsafe)

empty :: MonoidA f => U f
empty = A Rope.empty emptyA

append :: MonoidA f => Ann a f -> Ann b f -> U f
append (A r a) (A s b) = A (r `mappend` s) (appendA r a s b)

unit :: (ReducerA f, Reducer t Rope) => t -> U f
unit t = A r (unitA r) 
    where r = Reducer.unit t

splitAt :: BreakableA f => Int -> Ann a f -> (U f, U f)
splitAt n (A r a) = (A r b, A r c) 
    where (b, c) = splitAtA n r a

drop :: BreakableA f => Int -> Ann a f -> U f
drop n (A r a) = A r (dropA n r a)

take :: BreakableA f => Int -> Ann a f -> U f
take n (A r a) = A r (takeA n r a)

snoc :: (ReducerA f, Reducer t Rope) => Ann a f -> t -> U f
snoc (A r a) t = A r' (snocA (Rope.length r' - Rope.length r) r' a)
    where r' = Reducer.snoc r t 

cons :: (ReducerA f, Reducer t Rope) => t -> Ann a f -> U f
cons t (A r a) = A r' (consA (Rope.length r' - Rope.length r) r' a)
    where r' = Reducer.cons t r

break :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> (U f, U f)
break p (A r a) = (A x b, A y c) where
    (x,y) = Rope.break p r
    (b,c) = splitAtA (Rope.length x) r a

span :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> (U f, U f)
span p (A r a) = (A x b, A y c) where
    (x,y) = Rope.span p r
    (b,c) = splitAtA (Rope.length x) r a

takeWhile :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> U f
takeWhile p (A r a) = A x b where
    x = Rope.takeWhile p r
    b = takeA (Rope.length x) r a

dropWhile :: (BreakableA f, Breakable t) => (t -> Bool) -> Ann a f -> U f
dropWhile p (A r a) = A y c where
    y = Rope.dropWhile p r
    c = dropA (Rope.length r - Rope.length y) r a

uncons :: (BreakableA f, Unpackable t) => Ann a f -> Maybe (t, U f)
uncons (A r a) = case Rope.uncons r of
    Just (c,cs) -> Just (c, A cs (dropA (Rope.length r - Rope.length cs) r a))
    Nothing -> Nothing

unsnoc :: (BreakableA f, Unpackable t) => Ann a f -> Maybe (U f, t)
unsnoc (A r a) = case Rope.unsnoc r of
    Just (cs,c) -> Just (A cs (dropA (Rope.length cs) r a), c)
    Nothing -> Nothing
