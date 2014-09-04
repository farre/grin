module Core.Value where

type Offset = Integer

newtype Name
  = Name String

data Variable
  = Register Integer
  | VariableName Name

data Value
  = Number Integer
  | Variable Variable
  | Empty Value
  | Node Value [Value]
