{-# Language TypeFamilies #-}
module VarArgs where

import List

{-
This is Oleg Kiselyov's polyvariadic functions in Haskell but using
associated type synonyms, see
http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn
-}
class VarArgs r where
  type Elem r
  build :: List (Elem r) -> r

args :: VarArgs t => t
args = build empty

instance VarArgs (List a) where
  type Elem (List a) = a
  build l = l

instance (VarArgs r, Elem r ~ a) => VarArgs (a -> r) where
  type Elem (a -> r) = a
  build l x = build (append x l)
