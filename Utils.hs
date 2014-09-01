module Utils where

import Syntax

newtype Var = Var Variable

instance Pattern Var where
  fromPattern (Var v) = literal v
  pattern (s:_) = Var s

newtype Tag = T { fromTag :: Value }

var :: String -> Value
var = Variable . VariableName . Name

cnode :: Tag -> [Value] -> Value
cnode = Node . fromTag

vname :: String -> [Value] -> Value
vname = Node . var
