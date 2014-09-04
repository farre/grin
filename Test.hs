module Test where

import Core
import Core.Pretty
import Grin
import Utils

-- TODO(farre): Remove testing, start using QuickCheck!
data Foo = Foo Variable Variable

instance Literal Foo where
  literal (Foo v0 v1) = Node (var "foo") [literal v0, literal v1]

instance Pattern Foo where
  fromPattern (Foo v0 v1) = Node (var "foo") [literal v0, literal v1]
  pattern (s0:s1:_) = Foo s0 s1

instance Literal Integer where
  literal = Number

test :: Pattern a => Integer -> Grin a
test n = do
  Var v <- unit n
  unit v

test' :: Integer -> Grin Value
test' n = unit n >>= \(Var v) -> return (literal v)

test2 :: Integer -> Integer -> Grin Value
test2 m n = do
  Var v <- unit m
  Var w <- unit n
  Variable x <- unit (99 :: Integer)
  Var q <- unit w
  return (Variable x)

test3 :: Pattern a => Grin a
test3 = do
  Foo v0 v1 <- unit (Node (var "foo") [(number 3), (number 8)])
  Var v2 <- unit v1
  Foo v3 v4 <- unit (42 :: Integer)
  Var v5 <- unit v4
  unit v0

test4 :: Pattern a => Grin a
test4 = do
  Var x <- test (5 :: Integer)
  Var y <- test3
  unit y

test5 :: Pattern a => Grin a
test5 = do
  Var x <- unit (5 :: Integer)
  Var y <- store x
  Var z <- update y (5 :: Integer)
  unit y

test4' :: Pattern a => Grin a
test4' = test 5 >>= \(Var x) -> test3 >>= \(Var y) -> unit y

test6 :: Pattern a => Grin a
test6 = do
  Var x <- unit (5 :: Integer)
  Var y <- store x
  Var z <- (VariableName (Name "f")) $+ args (literal x) (literal y) (number 5)
  unit z

test7 :: Pattern a => Grin a
test7 = do
  Var x <- unit (5 :: Integer)
  Var y <- switch x $ args
             (match $ \(Var x) -> unit x)
             (match $ \(Foo a b) -> unit x)
  unit y

test8 :: Pattern a => Variable -> Grin a
test8 f = do
  Var x <- switch f $ args
             (match $ \(Foo a b) -> unit a)
  unit x

test9 :: Pattern a => Variable -> Variable -> Grin a
test9 a0 a1 = do
  Var x <- switch a0 $ args
             (match $ \(Foo a b) ->
                switch a1 $ args
                  (match $ \(Var y) -> unit y))
  unit x

runTest = pp . transform . interpret $ (test 5 :: Grin Value)

runTest' = pp . transform . interpret $ (test' 5)

runTest2 = pp . transform . interpret $ (test2 5 6)

runTest3 = pp . transform . interpret $ (test3 :: Grin Value)

runTest4 = pp . transform . interpret $ (test4 :: Grin Value)

runTest4' = pp . transform . interpret $ (test4' :: Grin Value)

runTest5 = pp . transform . interpret $ (test5 :: Grin Value)

runTest6 = pp . transform . interpret $ (test6 :: Grin Value)

runTest7 = pp . transform . interpret $ (test7 :: Grin Value)

runTest6' = pp $ declare (Name "foo") (test6 :: Grin Value)

runTest8 = pp $ declare (Name "foo") (test8 :: Variable -> Grin Value)

runTest9 = pp $ declare (Name "foo") (test9 :: Variable -> Variable -> Grin Value)
