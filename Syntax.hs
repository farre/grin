{-# Language GADTs #-}
module Syntax where

import Program

{-
  What follows is a deep embedding of GRIN as a DSL with a patterns.
  GRIN is by Urban Boquist 1999.
-}
data Value where
  Number   :: Integer -> Value
  Variable :: Variable -> Value
  Empty    :: Value
  Node     :: Value -> [Value] -> Value

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

data Declaration where
  Declaration :: Name
              -> [Value]
              -> Expression Value
              -> Declaration

class Literal a where
  literal :: a -> Value

class Pattern a where
  fromPattern :: a -> Value
  pattern :: [Variable] -> a

newtype Name = Name String

data Variable = Register Integer | VariableName Name

instance Literal Variable where
  literal = Variable

instance Pattern Variable where
  fromPattern = Variable
  pattern vs = head vs

instance Literal Value where
  literal = id

type Grin a = Program Expression a

type Offset = Integer
type Unique = Integer

