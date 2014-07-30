module List where

newtype List a = List ([a] -> [a])

append :: a -> List a -> List a
append x (List l) = List (l . (x :))

list :: List a -> [a]
list (List l) = l []

empty :: List a
empty = List id