module Core.Variable where

newtype Name
  = Name String

data Variable
  = Register Integer
  | VariableName Name
