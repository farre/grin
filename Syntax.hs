{-# Language GADTs #-}
module Syntax(
  module Syntax
) where

import Core.Variable
import qualified Core.Value as Core
import Core.Value ( Offset )
import Program

{-
  What follows is a deep embedding of GRIN as a DSL with a patterns.
  GRIN is by Urban Boquist 1999.
-}

class Valueable a where
  fromValue :: a -> Core.Value

data Value where
  Value :: Valueable a => a -> Value

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

instance Valueable Variable where
  fromValue = Core.Variable

instance Literal Variable where
  literal = Value

instance Literal Value where
  literal = id

instance Valueable Value where
  fromValue (Value v) = fromValue v

instance Valueable Core.Value where
  fromValue = id

instance Pattern Core.Value where
  fromPattern = Value
  pattern (s:_) = Core.Variable s

instance Literal Core.Value where
  literal = Value

type Grin a = Program Expression a

