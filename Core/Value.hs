module Core.Value where

import Core.Variable

type Offset = Integer

data Value
  = Number Integer
  | Variable Variable
  | Empty Value
  | Node Value [Value]
