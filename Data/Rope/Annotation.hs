{-# LANGUAGE TypeOperators #-}
module Data.Rope.Annotation
    ( MonoidalAnn(..)
    , PackableAnn(..)
    , BreakableAnn(..)
    ) where

import Data.Rope (Rope)

class MonoidalAnn f where
    -- | build an empty 'Annotation'
    emptyAnn   :: f a
    -- | append two annotations, based on their corresponding ropes
    appendAnn  :: Rope -> f a -> Rope -> f b -> f c

class MonoidalAnn f => PackableAnn f where
    -- | construct an 'Annotation' from a 'Rope' out of whole cloth
    packAnn    :: Rope -> f a
    -- | The 'Rope' has been updated to contains n more bytes on the right than the one used to build the 'Annotation', update the 'Annotation'
    snocAnn    :: Int -> Rope -> f a -> f b
    -- | The 'Rope' contains n more bytes on the left than the one used to build the 'Annotation', update the 'Annotation'
    consAnn    :: Int -> Rope -> f a -> f b
    
class BreakableAnn f where

    -- | split an 'Annotation' about a 'Rope' into two annotations, one about the first n bytes, the other about the remainder
    splitAtAnn :: Int -> Rope -> f a -> (f b, f c)
    -- | truncate the 'Annotation' to 'length' n
    takeAnn    :: Int -> Rope -> f a -> f b
    -- | drop the first n bytes from the 'Annotation'
    dropAnn    :: Int -> Rope -> f a -> f b

    takeAnn n r = fst . splitAtAnn n r
    dropAnn n r = snd . splitAtAnn n r
