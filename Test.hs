module Test where

import Grin
import Pretty

-- TODO(farre): Remove testing, start using QuickCheck!
data Foo = Foo Variable Variable

instance Pattern Foo where
  fromPattern (Foo v0 v1) = Node "foo" [toValue v0, toValue v1]
  pattern (s0:s1:_) = Foo s0 s1

instance Value Foo where
  toValue (Foo v0 v1) = Node "foo" [toValue v0, toValue v1]

test :: Pattern a => Integer -> Grin a
test n = do
  Var v <- unit n
  unit v

test' :: Integer -> Grin GrinValue
test' n = unit n >>= \(Var v) -> return (toValue v)

test2 :: Integer -> Integer -> Grin GrinValue
test2 m n = do
  Var v <- unit m
  Var w <- unit n
  Variable x <- unit (99 :: Integer)
  Var q <- unit w
  return (Variable x)

test3 :: Pattern a => Grin a
test3 = do
  Foo v0 v1 <- unit (Node "foo" [(number 3), (number 8)])
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
  Var z <- (VariableName (Name "f")) $+ args (toValue x) (toValue y) (number 5)
  unit z

test7 :: Pattern a => Grin a
test7 = do
  Var x <- unit (5 :: Integer)
  Var y <- switch x $ args
             (match $ \(Var x) -> unit x)
             (match $ \(Foo a b) -> unit x)
  unit y

test8 :: Pattern a => Var -> Grin a
test8 (Var f) = do
  Var x <- switch f $ args
             (match $ \(Foo a b) -> unit a)
  unit x

test9 :: Pattern a => Var -> Var -> Grin a
test9 (Var a0) (Var a1) = do
  Var x <- switch a0 $ args
             (match $ \(Foo a b) ->
                switch a1 $ args
                  (match $ \(Var y) -> unit y))
  unit x

runTest = pp $ interpret $ (test 5 :: Grin GrinValue)

runTest' = pp $ interpret $ (test' 5)

runTest2 = pp $ interpret $ (test2 5 6)

runTest3 = pp $ interpret $ (test3 :: Grin GrinValue)

runTest4 = pp $ interpret $ (test4 :: Grin GrinValue)

runTest4' = pp $ interpret $ (test4' :: Grin GrinValue)

runTest5 = pp $ interpret $ (test5 :: Grin GrinValue)

runTest6 = pp $ interpret $ (test6 :: Grin GrinValue)

runTest7 = pp $ interpret $ (test7 :: Grin GrinValue)

runTest6' = pp $ declare (Name "foo") (test6 :: Grin GrinValue)

runTest8 = pp $ declare (Name "foo") (test8 :: Var -> Grin GrinValue)

runTest9 = pp $ declare (Name "foo") (test9 :: Var -> Var -> Grin GrinValue)
