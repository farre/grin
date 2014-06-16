{-# Language GADTs, NoMonomorphismRestriction, NoMonoLocalBinds,
             RankNTypes #-}
module Grin where

import Control.Monad.ST.Lazy hiding (unsafeInterleaveST)
import Control.Monad.ST.Lazy.Unsafe

import Data.Function
import Data.List
import Data.STRef.Lazy

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
  pattern :: [Variable] -> a

data Variable = Register Integer | Name String

instance Value Variable where
  toValue = Variable

instance Show Variable where
  show (Register n) = 'x' : show n
  show (Name n) = n

data GrinValue where
  Number   :: Integer -> GrinValue
  Variable :: Variable -> GrinValue
  Empty    :: GrinValue
  Node     :: String -> [GrinValue] -> GrinValue

data Binding a b where
  Bind :: (Pattern a, Pattern b) => a -> GrinExpression b -> Binding a b

data GrinExpression a where
  Sequence    :: (Pattern a, Pattern b) =>
                 GrinExpression a -> Binding a b -> GrinExpression b
  Case        :: Pattern a => Variable -> [(GrinValue, GrinExpression GrinValue)] -> GrinExpression a

  Application :: Pattern a => Variable -> [GrinValue] -> GrinExpression a
  Unit        :: Pattern a => GrinValue -> GrinExpression a
  Store       :: Pattern a => GrinValue -> GrinExpression a
  Fetch       :: Pattern a => Variable  -> Maybe Offset -> GrinExpression a
  Update      :: Pattern a => Variable  -> GrinValue    -> GrinExpression a

  Switch      :: Pattern a => Variable -> [(GrinValue, Grin GrinValue)] -> GrinExpression a

type Grin a = Program GrinExpression a

type Offset = Integer
type Unique = Integer

number :: Integral a => a -> GrinValue
number = Number . toInteger

singleton :: instr a -> Program instr a
singleton i = i `Then` Return

($+) :: Pattern p => Variable -> [GrinValue] -> Grin p
($+) n = singleton . (Application n)

unit :: (Value v, Pattern p) => v -> Grin p
unit = singleton . Unit . toValue

store :: (Value v, Pattern p) => v -> Grin p
store = singleton . Store . toValue

fetch :: Pattern p => Variable -> Maybe Offset -> Grin p
fetch v o = singleton . (flip Fetch o) $ v

update :: (Value v, Pattern p) => Variable -> v -> Grin p
update v = singleton . (Update v) . toValue

switch :: Pattern p => Variable -> [(GrinValue, Grin GrinValue)] -> Grin p
switch v = singleton . (Switch v)

match :: Pattern a => Supply s Unique -> (a -> Grin b) -> (GrinValue, Grin b)
match s f = let (p, r) = bind s f in (fromPattern p, r)

bind :: Pattern a => Supply s Unique -> (a -> Grin b) -> (a, Grin b)
bind s f =
  let p = pattern . (map newRegister) . split $ s
  in (p, f p)

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
      let (p, e) = bind s f
      e' <- go (head (split s)) e
      return (Sequence x (Bind p e'))
    switchToCase :: Pattern a =>
                    Supply s Unique -> GrinExpression a -> ST s (GrinExpression a)
    switchToCase s (Switch v as) = do
      let (ps, es) = unzip as
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

instance Show GrinValue where
  show (Number n)   = show n
  show (Variable v) = show v
  show (Node n vs) = showString "(" .
                     showString n .
                     showString " " .
                     (flip (foldr id) $
                       ((intersperse (showString " ")) . (map shows)) vs) .
                     showString ")" $ ""

instance Show (Binding a b) where
  show (Bind v e) = "\\" ++ show (fromPattern v) ++ " -> " ++ show e

instance Show (GrinExpression a) where
  show (Sequence e b)     = show e ++ "; " ++ show b
  show (Application n vs) = show n ++ " " ++ concat (intersperse " " (map show vs))
  show (Unit v)           = "unit " ++ show v
  show (Store v)          = "store " ++ show v
  show (Fetch v n)        = "fetch " ++ show v ++ maybe "" (\n' -> "[" ++ show n' ++ "]") n
  show (Update v w)       = "update " ++ show v ++ " " ++ show w

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

runTest = interpret $ (test 5 :: Grin GrinValue)

runTest' = interpret $ (test' 5)

runTest2 = interpret $ (test2 5 6)

runTest3 = interpret $ (test3 :: Grin GrinValue)

runTest4 = interpret $ (test4 :: Grin GrinValue)

runTest4' = interpret $ (test4' :: Grin GrinValue)

runTest5 = interpret $ (test5 :: Grin GrinValue)

runTest6 = interpret $ (test6 :: Grin GrinValue)
