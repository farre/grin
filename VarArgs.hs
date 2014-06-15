{-# Language TypeFamilies, RankNTypes #-}
module VarArgs where

class VarArgs r where
  type Elem r
  build' :: [Elem r] -> Elem r -> r

args :: forall r a . (VarArgs r, Elem r ~ a) => a -> r
args x = build' [] x

instance VarArgs [a] where
  type Elem [a] = a
  build' l x = reverse $ x:l

instance (VarArgs r, Elem r ~ a) => VarArgs (a -> r) where
  type Elem (a -> r) = a
  build' l x y = build' (x:l) y