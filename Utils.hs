module Utils where

import Syntax

var :: String -> Value
var = Variable . VariableName . Name

cnode :: String -> [Value] -> Value
cnode = Node . var

vname :: String -> [Value] -> Value
vname = Node . var
