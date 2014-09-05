module Core.Syntax where

import Core.Value
import Core.Variable

data Expression
  = Sequence Expression Value Expression
  | Case Variable [(Value, Expression)]
  | Application Variable [Value]
  | Unit Value
  | Store Value
  | Fetch Variable (Maybe Offset)
  | Update Variable Value

data Declaration
  = Declaration Name [Variable] Expression
