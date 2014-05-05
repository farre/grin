{-# Language GADTs, NoMonomorphismRestriction, NoMonoLocalBinds, RankNTypes, FlexibleInstances #-}
module Grin where

import Control.Monad.ST.Lazy hiding (unsafeInterleaveST)
import Control.Monad.ST.Lazy.Unsafe

import Data.List
import Data.STRef.Lazy

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
      v  <- unsafeInterleaveST (modifySTRef r (\x -> fst (upd x)) >> readSTRef r)
      ls <- gen r
      rs <- gen r
      return (Supply v ls rs)
    upd a = let b = next a in seq b (b, a)

modifySupply :: Supply s a -> (Supply s a -> b) -> Supply s b
modifySupply s f = Supply (f s) (modifySupply l f) (modifySupply r f)
  where Supply _ l r = s

{-
  This is basically 'operational' as seen on hackage, by (c) Heinrich
  Apfelmus 2010-2011 (BSD3), modified for my purposes.
-}

-- TODO(farre): Move me to a separate file, or actually use operational.
data Program instr a where
  Then   :: instr a -> (a -> Program instr b) -> Program instr b
  Return :: a -> Program instr a

instance Monad (Program instr) where
  return = Return
  x >>= f = case x of
    Return i    -> f i
    i `Then` is -> i `Then` (\a -> is a >>= f)

{-
  What follows is a deep embedding of GRIN as a DSL with a patterns.
  GRIN is by Urban Boquist 1999.
-}   
class Value a where
  toValue :: a -> GrinValue

class Pattern a where
  fromPattern :: a -> GrinValue
  match :: [Supply s Unique] -> a

data GrinValue where
  Number   :: Integer -> GrinValue
  Variable :: String -> GrinValue
  Empty    :: GrinValue
  Node     :: String -> [GrinValue] -> GrinValue

data Binding a b where
  Bind :: (Pattern a, Pattern b) => a -> GrinExpression b -> Binding a b

data GrinExpression a where
  Sequence :: (Pattern a, Pattern b) => GrinExpression a -> Binding a b -> GrinExpression b
  Unit :: Pattern a => GrinValue -> GrinExpression a
 
type Grin a = Program GrinExpression a

type Unique = Integer

number :: Integral a => a -> GrinValue
number = Number . toInteger

singleton :: instr a -> Program instr a
singleton i = i `Then` Return

unit :: (Value v, Pattern p) => v -> Grin p
unit = singleton . Unit . toValue

interpret :: Pattern a => Grin a -> GrinExpression GrinValue
interpret e = runST (newSupply 0 (+1) >>= \s -> go s e)
  where
    go :: (Pattern a, Pattern b) => Supply s Unique -> Grin a -> ST s (GrinExpression b)
    go s (Return x)   = return . Unit . fromPattern $ x
    go s (x@(Unit v) `Then` f) = do
      let p = match (split s)
      e <- go (head (split s)) (f p)
      return (Sequence x (Bind p e))

freshVariable :: Supply s Unique -> GrinValue
freshVariable s = Variable ("x" ++ show (supplyValue s))

instance Value Integer where
  toValue = Number

instance Value GrinValue where
  toValue = id
  
newtype Var = Var GrinValue

instance Pattern Var where
  fromPattern (Var v) = v
  match (s:_) = Var (freshVariable s)

data Foo = Foo GrinValue GrinValue

instance Pattern Foo where
  fromPattern (Foo v0 v1) = Node "foo" [v0, v1]
  match (s0:s1:_) = Foo (freshVariable s0) (freshVariable s1)

instance Value Foo where
  toValue (Foo v0 v1) = Node "foo" [v0, v1]
  
instance Pattern GrinValue where
  fromPattern = id
  match (s:_) = freshVariable s

instance Show GrinValue where
  show (Number n)   = show n
  show (Variable v) = v
  show (Node n vs) = "(" ++ n ++ " " ++ concat (intersperse " " (map show vs)) ++ ")"

instance Show (Binding a b) where
  show (Bind v e) = "\\" ++ show (fromPattern v) ++ " -> " ++ show e

instance Show (GrinExpression a) where
  show (Unit v) = "unit " ++ show v
  show (Sequence e b) = show e ++ "; " ++ show b
      
-- TODO(farre): Remove testing, start using QuickCheck!
test :: Pattern a => Integer -> Grin a
test n = do
  Var v <- unit n
  unit v
  
test' :: Integer -> Grin GrinValue
test' n = unit n >>= \(Var v) -> return v
  
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

runTest = interpret $ (test 5 :: Grin GrinValue)

runTest' = interpret $ (test' 5)

runTest2 = interpret $ (test2 5 6)

runTest3 = interpret $ (test3 :: Grin GrinValue)

runTest4 = interpret $ (test4 :: Grin GrinValue)