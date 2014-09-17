{-# Language GADTs, NoMonomorphismRestriction, NoMonoLocalBinds,
             RankNTypes, TypeSynonymInstances, FlexibleInstances,
             RecordWildCards, MultiParamTypeClasses, FlexibleContexts #-}
module Grin (
  module Grin,
  module Syntax,
  module VarArgs
) where

import Control.Monad.ST.Lazy
import Data.Monoid

import Core ( transform, Declaration(..), Variable(..), Offset, Name(..) )
import qualified Core.Value as V
import List
import Program
import Syntax
import Unique
import VarArgs

type Unique s = Supply s Integer

number :: Integral a => a -> Value
number = Value . V.Number . toInteger

singleton :: instr a -> Program instr a
singleton i = i `Then` Return

($+) :: Pattern p => Variable -> Terminate (List Value) -> Grin p
($+) n = singleton . (Application n) . list . terminate

unit :: (Literal v, Pattern p) => v -> Grin p
unit = singleton . Unit . literal

unit' :: Literal v => v -> Grin Value
unit' = unit

store :: (Literal v, Pattern p) => v -> Grin p
store = singleton . Store . literal

store' :: Literal v => v -> Grin Value
store' = store

fetch :: Pattern p => Variable -> Maybe Offset -> Grin p
fetch v o = singleton . (flip Fetch o) $ v

fetch' :: Variable -> Maybe Offset -> Grin Value
fetch' = fetch

update :: (Literal v, Pattern p) => Variable -> v -> Grin p
update v = singleton . (Update v) . literal

update' :: Literal v => Variable -> v -> Grin Value
update' = update

switch :: Pattern p => Variable -> Terminate (List Alternative) -> Grin p
switch v = singleton . (Switch v) . list . terminate

switch' :: Variable -> Terminate (List Alternative) -> Grin Value
switch' = switch

on :: CPolyVariadic (List Alternative) r => r
on = ctm (mempty :: List Alternative)

args :: CPolyVariadic (List Value) r => r
args = ctm (mempty :: List Value)

match :: (Pattern p, Pattern p') => (p -> Grin p') -> [Variable] -> (Value, Grin p')
match f vs = let (p, e) = bind vs f in (fromPattern p, f p)

bind :: Pattern a => [Variable] -> (a -> b) -> (a, b)
bind vs f = let p = pattern vs in (p, f p)

newVariables :: Unique s -> [Variable]
newVariables = (map newRegister) . splits

newRegister :: Unique s -> Variable
newRegister = Register . supplyValue

interpret :: Pattern a => Grin a -> Expression Value
interpret e = runST (newSupply 0 (+1) >>= \s -> interpret' s e)

interpret' :: Pattern a => Unique s -> Grin a -> ST s (Expression Value)
interpret' s e = go s e
  where
    go :: (Pattern a, Pattern b) =>
          Unique s -> Grin a -> ST s (Expression b)
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
                    Unique s -> Expression a -> ST s (Expression a)
    switchToCase s (Switch v as) = do
      let (s0, s1) = split s
          (ps, es) = unzip . (zipWith (($) . unalternative) as) . (map newVariables) . splits $ s0
      es' <- mapM (uncurry go) $ zip (splits s1) es
      return (Case v (zip ps es'))

class Declarable a where
  buildDeclaration :: Name
                   -> List Variable
                   -> Unique s
                   -> a
                   -> ST s Declaration

instance Declarable (Grin V.Value) where
  buildDeclaration n l u g = fmap ((Declaration n (list l)) . transform) $ interpret' u g

instance Declarable b => Declarable (Variable -> b) where
  buildDeclaration n l s f =
    let (s0, s1) = split s
        p:_ = newVariables s0
    in buildDeclaration n (append p l) s1 (f p)

declare :: Declarable d => Name -> d -> Declaration
declare name decl = runST (newSupply 0 (+1) >>= \s -> buildDeclaration name empty s decl)

instance (Pattern p, Pattern p') => Monoidable (p -> Grin p') (List Alternative) where
  toMonoid = flip append empty . Alternative . match
