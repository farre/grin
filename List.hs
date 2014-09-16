module List where

import Data.Monoid

instance Monoid (List a) where
  mempty = empty
  mappend a b = List $ unlist a . unlist b

newtype List a = List { unlist :: [a] -> [a] }

append :: a -> List a -> List a
append x (List l) = List (l . (x :))

list :: List a -> [a]
list l = unlist l []

empty :: List a
empty = List id