{-# Language GADTs #-}
module Syntax(
  module Core.Value,
  module Syntax
) where

import Core.Value
import Program

{-
  What follows is a deep embedding of GRIN as a DSL with a patterns.
  GRIN is by Urban Boquist 1999.
-}

data Binding a b where
  Bind :: (Pattern a, Pattern b) => a -> Expression b -> Binding a b

type Alternative = [Variable] -> (Value, Grin Value)

data Expression a where
  Sequence    :: (Pattern a, Pattern b) =>
                 Expression a -> Binding a b -> Expression b
  Case        :: Pattern a => Variable -> [(Value, Expression Value)] -> Expression a

  Application :: Pattern a => Variable -> [Value] -> Expression a
  Unit        :: Pattern a => Value -> Expression a
  Store       :: Pattern a => Value -> Expression a
  Fetch       :: Pattern a => Variable  -> Maybe Offset -> Expression a
  Update      :: Pattern a => Variable  -> Value    -> Expression a

  Switch      :: Pattern a => Variable -> [Alternative] -> Expression a

class Literal a where
  literal :: a -> Value

class Pattern a where
  fromPattern :: a -> Value
  pattern :: [Variable] -> a

instance Pattern Value where
  fromPattern = id
  pattern (s:_) = literal s

instance Literal Variable where
  literal = Variable

instance Literal Value where
  literal = id

type Grin a = Program Expression a

type Unique = Integer

