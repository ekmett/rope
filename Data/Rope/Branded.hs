{-# LANGUAGE TypeOperators, Rank2Types, EmptyDataDecls, 
             MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             IncoherentInstances, OverlappingInstances #-}

module Data.Rope.Branded
    ( Branded(..)
    , Unsafe
    , UBR
    , null      -- :: (s `Branded` Rope) a -> Bool
    -- * Unpacking Ropes
    , head      -- :: Unpackable t => (s `Branded` Rope) a -> t
    , last      -- :: Unpackable t => (s `Branded` Rope) a -> t
    , unpack    -- :: Unpackable t => (s `Branded` Rope) a -> [t]
    -- * MonadWriter
    , runBranded
    , execBranded -- MonadWriter terminology for 'context'
    ) where

import Prelude hiding (null, head, last, take, drop, span, break, splitAt, takeWhile, dropWhile)

import Control.Applicative hiding (empty)
import Control.Monad.Writer.Class

import Data.Rope.Branded.Comonad
import Data.Monoid
import Data.FingerTree (Measured(..))
import Data.Foldable (Foldable)
import qualified Data.Foldable
import Data.Traversable (Traversable(traverse))
import qualified Data.Rope.Internal as Rope
import Data.Rope.Internal (Rope(..),Unpackable)

type UBR a = (Unsafe `Branded` Rope) a

data Unsafe

data Branded brand t a = Branded { context :: !t, extractBranded :: a }

null :: Branded s Rope a -> Bool
null = Rope.null . context
{-# INLINE null #-} 

head :: Unpackable t => Branded s Rope a -> t
head = Rope.head . context
{-# INLINE head #-}

last :: Unpackable t => Branded s Rope a -> t
last = Rope.last . context
{-# INLINE last #-}

unpack :: Unpackable t => Branded s Rope a -> [t]
unpack (Branded s _) = Rope.unpack s
{-# INLINE unpack #-}

instance Measured v t => Measured v (Branded s t a) where
    measure = measure . context 

instance Functor (Branded s t) where
    fmap f (Branded s a) = Branded s (f a) 

instance Comonad (Branded s t) where
    extract = extractBranded
    extend f a@(Branded s _) = Branded s (f a)
    duplicate a@(Branded s _) = Branded s a

instance Foldable (Branded s t) where
    foldr f z (Branded _ a) = f a z
    foldr1 _ (Branded _ a) = a
    foldl f z (Branded _ a) = f z a
    foldl1 _ (Branded _ a) = a
    foldMap f (Branded _ a) = f a

instance Traversable (Branded s t) where
    traverse f (Branded s a) = Branded s <$> f a

instance Monoid t => Applicative (Branded Unsafe t) where
    pure = Branded mempty
    Branded s f <*> Branded s' a = Branded (s `mappend` s') (f a)

instance Monoid t => Monad (Branded Unsafe t) where
    return = Branded mempty
    Branded s a >>= f = Branded (s `mappend` s') b
        where Branded s' b = f a

instance (Monoid t, Monoid m) => Monoid (Branded Unsafe t m) where
    mempty = Branded mempty mempty
    Branded r t `mappend` Branded s u = Branded (r `mappend` s) (t `mappend` u)

-- > sample :: Branded Unsafe Rope ()
-- > sample = do pack "Hello"
-- >             pack ' '
-- >             pack "World"
-- > 
instance Monoid t => MonadWriter t (Branded Unsafe t) where
    tell t = Branded t ()
    listen (Branded t a) = Branded t (a, t)
    pass (Branded t (a,f)) = Branded (f t) a

runBranded :: Branded s t a -> (a, t)
runBranded (Branded t a) = (a, t)
{-# INLINE runBranded #-}

execBranded :: Branded s t a -> t
execBranded (Branded t _) = t 
{-# INLINE execBranded #-}
