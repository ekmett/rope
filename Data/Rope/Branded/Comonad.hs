module Data.Rope.Branded.Comonad 
    ( Comonad(..)
    ) where

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b

    duplicate = extend id
    extend f = fmap f . duplicate
