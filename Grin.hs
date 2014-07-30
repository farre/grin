{-# Language GADTs, NoMonomorphismRestriction, NoMonoLocalBinds,
             RankNTypes, TypeSynonymInstances, FlexibleInstances,
             RecordWildCards  #-}
module Grin (
  module Grin,
  module Syntax,
  module VarArgs
) where

import Control.Monad.ST.Lazy
import Data.Function

import List
import Program
import Syntax
import Unique
import VarArgs

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

bind :: Pattern a => [Variable] -> (a -> b) -> (a, b)
bind vs f = let p = pattern vs in (p, f p)

newVariables :: Supply s Unique -> [Variable]
newVariables = (map newRegister) . splits

newRegister :: Supply s Unique -> Variable
newRegister = Register . supplyValue

interpret :: Pattern a => Grin a -> Expression GrinValue
interpret e = runST (newSupply 0 (+1) >>= \s -> interpret' s e)

interpret' :: Pattern a => Supply s Unique -> Grin a -> ST s (Expression GrinValue)
interpret' s e = go s e
  where
    go :: (Pattern a, Pattern b) =>
          Supply s Unique -> Grin a -> ST s (Expression b)
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
      let (s0, s1) = split s
          (p, e) = bind (newVariables s0) f
      e' <- go s1 e
      return (Sequence x (Bind p e'))
    switchToCase :: Pattern a =>
                    Supply s Unique -> Expression a -> ST s (Expression a)
    switchToCase s (Switch v as) = do
      let (s0, s1) = split s
          (ps, es) = unzip . (zipWith ($) as) . (map newVariables) . splits $ s0
      es' <- mapM (uncurry go) $ zip (splits s1) es
      return (Case v (zip ps es'))

instance Value Integer where
  toValue = Number

instance Value GrinValue where
  toValue = id

newtype Var = Var Variable

instance Pattern Var where
  fromPattern (Var v) = toValue v
  pattern (s:_) = Var s

instance Pattern GrinValue where
  fromPattern = id
  pattern (s:_) = toValue s

class Declarable a where
  buildDeclaration :: Name
                   -> List GrinValue
                   -> Supply s Unique
                   -> a
                   -> ST s Declaration

instance Declarable (Grin GrinValue) where
  buildDeclaration n l u g = fmap (Declaration n (list l)) $ interpret' u g

instance (Pattern a, Declarable b) => Declarable (a -> b) where
  buildDeclaration n l s f =
    let (s0, s1) = split s
        (p, d) = bind (newVariables s0) f
    in buildDeclaration n (append (fromPattern p) l) s1 d

declare :: Declarable d => Name -> d -> Declaration
declare name decl = runST (newSupply 0 (+1) >>= \s -> buildDeclaration name empty s decl)
