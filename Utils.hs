module Utils where

import Syntax

var :: String -> GrinValue
var = Variable . VariableName . Name

cnode :: String -> [GrinValue] -> GrinValue
cnode = Node . var

vname :: String -> [GrinValue] -> GrinValue
vname = Node . var