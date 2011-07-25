{-# LANGUAGE TypeOperators, Rank2Types, EmptyDataDecls, 
             MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             TypeFamilies, IncoherentInstances, OverlappingInstances #-}
module Data.Rope.Annotated
    ( -- * Annotated 'Rope's 
      Branded(context)   
    , Ann
    , MonoidalAnn, PackableAnn, BreakableAnn 
    , runAnn     -- :: Ann f a -> (forall b. Ann b f -> r) -> r

      -- * Unpacking 'Ropes'
    , null      -- :: (s `Branded` Rope) a -> Bool
    , head      -- :: Unpackable t => (s `Branded` Rope) a -> t
    , last      -- :: Unpackable t => (s `Branded` Rope) a -> t
    , unpack    -- :: Unpackable t => (s `Branded` Rope) a -> [t]

      -- * Building Annotated 'Rope'
    , empty     -- :: MonoidalAnn f => Ann Empty f 
    , append    -- :: MonoidalAnn f => Ann f a -> Ann f b -> Ann f (a :<> b)

    , pack      -- :: (PackableAnn f, Packable t) => t -> (forall a. Ann f (Return a) -> r) -> r
    , snoc      -- :: (PackableAnn f, Packable t) => t -> Ann f a -> (forall c. Ann f (Snoc c t a) -> r) -> r
    , cons      -- :: (PackableAnn f, Packable t) => Ann f a -> t -> (forall c. Ann f (Cons c t a) -> r) -> r

      -- * Cutting An Annotated 'Rope'
    , splitAt   -- :: (BreakablaAnn f) => Int -> Ann f a -> (forall n. Ann f (Take n a) -> Ann f (Drop n a) -> r) -> r
    , drop      -- :: (BreakableAnn f) => Int -> Ann f a -> (forall n. Ann f (Drop n a) -> r) -> r
    , take      -- :: (BreakablaAnn f) => Int -> Ann f a -> (forall n. Ann f (Take n a) -> r) -> r

    , break     -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Take n a) -> Ann f (Drop n a) -> r) -> r
    , span      -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Take n a) -> Ann f (Drop n a) -> r) -> r
    , takeWhile -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Take n a) -> r) -> r
    , dropWhile -- :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Drop n a) -> r) -> r

    -- * Inspecting the ends of the 'Rope'
    , uncons    -- :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (t, Ann f (Tail t b))
    , unsnoc    -- :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (Ann f (Init b t), t)

    -- * Type-level constructors
    , Drop, Take, Snoc, Cons, Tail, Init, Return, (:<>)
    , Tailed, Inited, Dropped, Taken, Nil, (:>)

    -- * Annotations
    -- ** Annotation Product
    , (:*:)(..)
    , fstF      -- :: (f :*: g) a -> f a 
    , sndF      -- :: (f :*: g) a -> g a
    -- ** Annotation Unit
    , Unit
    ) where

import Prelude hiding (null, head, last, take, drop, span, break, splitAt, takeWhile, dropWhile)
import Data.Monoid

import qualified Data.Rope.Internal as Rope

import Data.Rope.Branded (Branded(..), null, head, last, unpack)
import Data.Rope.Annotation
import Data.Rope.Annotation.Product
import Data.Rope.Annotation.Unit

import Data.Rope.Internal (Rope(..), Breakable, Unpackable, Packable)

type Ann f s = (s `Branded` Rope) (f s)

data Nil
data a :> b 

type family a :<> b :: *
type instance (a :> b) :<> c = a :> (b :<> c)
type instance Nil :<> c = c 
type Return a = a :> Nil

data Taken n a
type family Take n a :: *
type instance Take n Nil = Nil
type instance Take n (a :> b) = Return (Taken n (a :> b))

data Dropped n a 
type family Drop n a :: *
type instance Drop n Nil = Nil
type instance Drop n (a :> b) = Return (Dropped n (a :> b))

data Token s t 
type Cons s t a = Token s t :> a
type Snoc a s t = a :<> Return (Token s t)

data Tailed t a 
type Tail t a = Return (Tailed t a)

data Inited a t
type Init a t = Return (Inited a t)

runAnn :: Ann f a -> (forall b. Ann f b -> r) -> r
runAnn a k = k a 

empty :: MonoidalAnn f => Ann f Nil
empty = Branded Rope.empty emptyAnn

append :: MonoidalAnn f => Ann f a -> Ann f b -> Ann f (a :<> b)
append (Branded r a) (Branded s b) = Branded (r `mappend` s) (appendAnn r a s b)

pack :: (PackableAnn f, Packable t) => t -> (forall a. Ann f (Return a) -> r) -> r
pack t k = k (Branded r (packAnn r)) 
    where 
        r :: Rope
        r = Rope.pack t

splitAt :: BreakableAnn f => Int -> Ann f a -> (forall n. Ann f (Take n a) -> Ann f (Drop n a) -> r) -> r
splitAt n (Branded r a) k = k (Branded r b) (Branded r c) 
    where (b, c) = splitAtAnn n r a

drop :: BreakableAnn f => Int -> Ann f a -> (forall n. Ann f (Drop n a) -> r) -> r
drop n (Branded r a) k = k (Branded r (dropAnn n r a))

take :: BreakableAnn f => Int -> Ann f a -> (forall n. Ann f (Take n a) -> r) -> r
take n (Branded r a) k = k (Branded r (takeAnn n r a))

snoc :: (PackableAnn f, Packable t) => Ann f a -> t -> (forall c. Ann f (Snoc a c t) -> r) -> r
snoc (Branded r a) t k = k (Branded r' (snocAnn (Rope.length r' - Rope.length r) r' a))
    where r' = Rope.snoc r t 

cons :: (PackableAnn f, Packable t) => t -> Ann f a -> (forall c. Ann f (Cons c t a) -> r) -> r
cons t (Branded r a) k = k (Branded r' (consAnn (Rope.length r' - Rope.length r) r' a))
    where r' = Rope.cons t r

break :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Take n a) -> Ann f (Drop n a) -> r) -> r
break p (Branded r a) k = k (Branded x b) (Branded y c) where
    (x,y) = Rope.break p r
    (b,c) = splitAtAnn (Rope.length x) r a

span :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Take n a) -> Ann f (Drop n a) -> r) -> r
span p (Branded r a) k = k (Branded x b) (Branded y c) where
    (x,y) = Rope.span p r
    (b,c) = splitAtAnn (Rope.length x) r a

takeWhile :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Take n a) -> r) -> r
takeWhile p (Branded r a) k = k (Branded x b) where
    x = Rope.takeWhile p r
    b = takeAnn (Rope.length x) r a

dropWhile :: (BreakableAnn f, Breakable t) => (t -> Bool) -> Ann f a -> (forall n. Ann f (Drop n a) -> r) -> r
dropWhile p (Branded r a) k = k (Branded y c) where
    y = Rope.dropWhile p r
    c = dropAnn (Rope.length r - Rope.length y) r a

uncons :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (t, Ann f (Tail t a))
uncons (Branded r a) = case Rope.uncons r of
    Just (c,cs) -> Just (c, Branded cs (dropAnn (Rope.length r - Rope.length cs) r a))
    Nothing -> Nothing

unsnoc :: (BreakableAnn f, Unpackable t) => Ann f a -> Maybe (Ann f (Init a t), t)
unsnoc (Branded r a) = case Rope.unsnoc r of
    Just (cs,c) -> Just (Branded cs (dropAnn (Rope.length cs) r a), c)
    Nothing -> Nothing

