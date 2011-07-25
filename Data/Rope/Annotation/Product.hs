{-# LANGUAGE TypeOperators #-}
module Data.Rope.Annotation.Product
    ( (:*:)(..)
    , fstF
    , sndF
    ) where

import Control.Applicative hiding (empty)

import Data.Monoid (mappend)
import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable
import Data.Traversable (Traversable(traverse))

import Data.Rope.Annotation

infixr 5 :*:

-- | A 'Rope' 'Annotation' product.
data (f :*: g) a = f a :*: g a

fstF :: (f :*: g) a -> f a 
fstF ~(f :*: _) = f

sndF :: (f :*: g) a -> g a
sndF ~(_ :*: g) = g

instance (Functor f, Functor g)  => Functor (f :*: g) where
    fmap f (a :*: b) = fmap f a :*: fmap f b

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
    pure a = pure a :*: pure a
    (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
    foldMap f (a :*: b) = foldMap f a `mappend` foldMap f b
    
instance (Traversable f, Traversable g) => Traversable (f :*: g) where
    traverse f (a :*: b) = (:*:) <$> traverse f a <*> traverse f b

instance (MonoidalAnn f, MonoidalAnn g) => MonoidalAnn (f :*: g) where
    emptyAnn = emptyAnn :*: emptyAnn
    appendAnn r (a :*: a') s (b :*: b') = 
        appendAnn r a s b :*: appendAnn r a' s b'

instance (PackableAnn f, PackableAnn g) => PackableAnn (f :*: g) where
    packAnn r = packAnn r :*: packAnn r
    snocAnn r n (f :*: g) = snocAnn r n f :*: snocAnn r n g
    consAnn n r (f :*: g) = consAnn n r f :*: consAnn n r g

instance (BreakableAnn f, BreakableAnn g) => BreakableAnn (f :*: g) where
    dropAnn n r (f :*: g) = dropAnn n r f :*: dropAnn n r g
    takeAnn n r (f :*: g) = takeAnn n r f :*: takeAnn n r g
    splitAtAnn n r (f :*: g) = (f' :*: g' , f'' :*: g'') where
        (f',f'') = splitAtAnn n r f
        (g',g'') = splitAtAnn n r g
