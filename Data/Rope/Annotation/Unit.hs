{-# LANGUAGE TypeOperators, EmptyDataDecls #-}
module Data.Rope.Annotation.Unit
    ( Unit
    ) where

import Data.Rope.Annotation

data Unit a

instance MonoidalAnn Unit where
    emptyAnn = undefined
    appendAnn _ _ _ _ = undefined

instance PackableAnn Unit where
    packAnn _ = undefined
    snocAnn _ _ _ = undefined
    consAnn _ _ _ = undefined

instance BreakableAnn Unit where
    takeAnn _ _ _ = undefined
    dropAnn _ _ _ = undefined
    splitAtAnn _ _ _ = (undefined, undefined)
