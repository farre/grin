{-# Language MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module VarArgs where

import Data.Monoid

{-
This is basically half of the polyToMonoid package as seen on Hackage.
-}
class Monoid m => Monoidable a m where
  toMonoid :: a -> m

squish :: Monoidable a m => m -> a -> m
squish m a = (m `mappend` (toMonoid a))

class Monoid m => CPolyVariadic m r where
  ctm :: m -> r

newtype Terminate m = Terminate { terminate :: m }

instance Monoid m => Monoid (Terminate m) where
  mempty = Terminate mempty
  mappend a b = Terminate $ (terminate a) `mappend` (terminate b)

instance (Monoid m', m' ~ m) => CPolyVariadic m (Terminate m') where
  ctm acc = Terminate acc

instance (Monoidable a m, CPolyVariadic m r) => CPolyVariadic m (a->r) where
  ctm acc = \a -> ctm (squish acc a)
