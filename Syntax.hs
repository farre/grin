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
  Bind :: (Pattern a, Pattern b) => a -> GrinExpression b -> Binding a b

type Alternative = [Variable] -> (GrinValue, Grin GrinValue)

data GrinExpression a where
  Sequence    :: (Pattern a, Pattern b) =>
                 GrinExpression a -> Binding a b -> GrinExpression b
  Case        :: Pattern a => Variable -> [(GrinValue, GrinExpression GrinValue)] -> GrinExpression a

  Application :: Pattern a => Variable -> [GrinValue] -> GrinExpression a
  Unit        :: Pattern a => GrinValue -> GrinExpression a
  Store       :: Pattern a => GrinValue -> GrinExpression a
  Fetch       :: Pattern a => Variable  -> Maybe Offset -> GrinExpression a
  Update      :: Pattern a => Variable  -> GrinValue    -> GrinExpression a

  Switch      :: Pattern a => Variable -> [Alternative] -> GrinExpression a

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

type Grin a = Program GrinExpression a

type Offset = Integer
type Unique = Integer

