module Utils where

import Syntax

newtype Tag = T { fromTag :: Value }

var :: String -> Value
var = Variable . VariableName . Name

cnode :: Tag -> [Value] -> Value
cnode = Node . fromTag

vname :: String -> [Value] -> Value
vname = Node . var
