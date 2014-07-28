{-# Language GADTs, NoMonomorphismRestriction, NoMonoLocalBinds,
             RankNTypes #-}
module Grin where

import Control.Monad.ST.Lazy hiding (unsafeInterleaveST)
import Control.Monad.ST.Lazy.Unsafe

import Data.Function
import Data.STRef.Lazy

import List
import Program
import Syntax
import Pretty
import VarArgs

{-
  This is ''On Generating Unique Names'', by Lennart Augustsson,
  Mikael Rittri, and Dan Synek; but for STRef. Borrowed from Data.Supply,
  (c) Iavor S. Diatchki, 2007 (BSD3), modified for my purposes.
-}

-- TODO(farre): Move me to a separate file.
data Supply s a = Supply a (Supply s a) (Supply s a)

supplyValue :: Supply s a -> a
supplyValue (Supply a _ _) = a

split :: Supply s a -> [Supply s a]
split (Supply _ s1 s2)  = s1 : split s2

-- TODO(farre): Is the unsafeInterleaveST safely unsafe?
newSupply :: a -> (a -> a) -> ST s (Supply s a)
newSupply start next = gen =<< newSTRef start
  where
    gen r = unsafeInterleaveST $ do
      v  <- unsafeInterleaveST (modifySTRef r upd >> readSTRef r)
      ls <- gen r
      rs <- gen r
      return (Supply v ls rs)
    upd a = let b = next a in seq b b

modifySupply :: Supply s a -> (Supply s a -> b) -> Supply s b
modifySupply s f = Supply (f s) (modifySupply l f) (modifySupply r f)
  where Supply _ l r = s

number :: Integral a => a -> GrinValue
number = Number . toInteger

singleton :: instr a -> Program instr a
singleton i = i `Then` Return

($+) :: Pattern p => Variable -> List GrinValue -> Grin p
($+) n = singleton . (Application n) . list

unit :: (Value v, Pattern p) => v -> Grin p
unit = singleton . Unit . toValue

store :: (Value v, Pattern p) => v -> Grin p
store = singleton . Store . toValue

fetch :: Pattern p => Variable -> Maybe Offset -> Grin p
fetch v o = singleton . (flip Fetch o) $ v

update :: (Value v, Pattern p) => Variable -> v -> Grin p
update v = singleton . (Update v) . toValue

switch :: Pattern p => Variable -> List Alternative -> Grin p
switch v = singleton . (Switch v) . list

match :: Pattern a => (a -> Grin b) -> [Variable] -> (GrinValue, Grin b)
match f vs = let (p, e) = bind vs f in (fromPattern p, f p)

bind :: Pattern a => [Variable] -> (a -> Grin b) -> (a, Grin b)
bind vs f = let p = pattern vs in (p, f p)

newVariables :: Supply s Unique -> [Variable]
newVariables = (map newRegister) . split

interpret :: Pattern a => Grin a -> GrinExpression GrinValue
interpret e = runST (newSupply 0 (+1) >>= \s -> go s e)
  where
    go :: (Pattern a, Pattern b) =>
          Supply s Unique -> Grin a -> ST s (GrinExpression b)
    go s (Return x)    = return . Unit . fromPattern $ x
    go s (x@(Switch        {}) `Then` f) = do
      x' <- switchToCase s x
      go' s x' f
    go s (x@(Application   {}) `Then` f) = go' s x f
    go s (x@(Unit          {}) `Then` f) = go' s x f
    go s (x@(Store         {}) `Then` f) = go' s x f
    go s (x@(Fetch         {}) `Then` f) = go' s x f
    go s (x@(Update        {}) `Then` f) = go' s x f
    go' s x f = do
      let (p, e) = bind (newVariables s) f
      e' <- go (head (split s)) e
      return (Sequence x (Bind p e'))
    switchToCase :: Pattern a =>
                    Supply s Unique -> GrinExpression a -> ST s (GrinExpression a)
    switchToCase s (Switch v as) = do
      let (ps, es) = unzip . (zipWith ($) as) . (map newVariables) . split $ s
      es' <- mapM (go s) es
      return (Case v (zip ps es'))

newRegister :: Supply s Unique -> Variable
newRegister = Register . supplyValue

instance Value Integer where
  toValue = Number

instance Value GrinValue where
  toValue = id

newtype Var = Var Variable

instance Pattern Var where
  fromPattern (Var v) = toValue v
  pattern (s:_) = Var s

data Foo = Foo Variable Variable

instance Pattern Foo where
  fromPattern (Foo v0 v1) = Node "foo" [toValue v0, toValue v1]
  pattern (s0:s1:_) = Foo s0 s1

instance Value Foo where
  toValue (Foo v0 v1) = Node "foo" [toValue v0, toValue v1]

instance Pattern GrinValue where
  fromPattern = id
  pattern (s:_) = toValue s

-- TODO(farre): Remove testing, start using QuickCheck!
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
  Var z <- (Name "f") $+ args (toValue x) (toValue y) (number 5)
  unit z

test7 :: Pattern a => Grin a
test7 = do
  Var x <- unit (5 :: Integer)
  Var y <- switch (Name "l") $ args
             (match $ \(Var x) -> unit x)
             (match $ \(Foo a b) -> unit x)
  unit y

runTest = interpret $ (test 5 :: Grin GrinValue)

runTest' = interpret $ (test' 5)

runTest2 = interpret $ (test2 5 6)

runTest3 = interpret $ (test3 :: Grin GrinValue)

runTest4 = interpret $ (test4 :: Grin GrinValue)

runTest4' = interpret $ (test4' :: Grin GrinValue)

runTest5 = interpret $ (test5 :: Grin GrinValue)

runTest6 = interpret $ (test6 :: Grin GrinValue)

runTest7 = interpret $ (test7 :: Grin GrinValue)
