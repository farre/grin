{-# Language GADTs #-}
module Syntax where

import Program

{-
  What follows is a deep embedding of GRIN as a DSL with a patterns.
  GRIN is by Urban Boquist 1999.
-}
data GrinValue where
  Number   :: Integer -> GrinValue
  Variable :: Variable -> GrinValue
  Empty    :: GrinValue
  Node     :: String -> [GrinValue] -> GrinValue

data Binding a b where
  Bind :: (Pattern a, Pattern b) => a -> Expression b -> Binding a b

type Alternative = [Variable] -> (GrinValue, Grin GrinValue)

data Expression a where
  Sequence    :: (Pattern a, Pattern b) =>
                 Expression a -> Binding a b -> Expression b
  Case        :: Pattern a => Variable -> [(GrinValue, Expression GrinValue)] -> Expression a

  Application :: Pattern a => Variable -> [GrinValue] -> Expression a
  Unit        :: Pattern a => GrinValue -> Expression a
  Store       :: Pattern a => GrinValue -> Expression a
  Fetch       :: Pattern a => Variable  -> Maybe Offset -> Expression a
  Update      :: Pattern a => Variable  -> GrinValue    -> Expression a

  Switch      :: Pattern a => Variable -> [Alternative] -> Expression a

data Declaration where
  Declaration :: [GrinValue] -> Expression GrinValue -> Declaration

class Value a where
  toValue :: a -> GrinValue

class Pattern a where
  fromPattern :: a -> GrinValue
  pattern :: [Variable] -> a

data Variable = Register Integer | Name String

instance Value Variable where
  toValue = Variable

instance Show Variable where
  show (Register n) = 'x' : show n
  show (Name n) = n

type Grin a = Program Expression a

type Offset = Integer
type Unique = Integer

