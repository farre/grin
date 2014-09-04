module Core.Syntax where

import Core.Value

data Expression
  = Sequence Expression Value Expression
  | Case Variable [(Value, Expression)]
  | Application Variable [Value]
  | Unit Value
  | Store Value
  | Fetch Variable (Maybe Offset)
  | Update Variable Value

data Declaration
  = Declaration Name [Value] Expression
