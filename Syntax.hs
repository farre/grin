{-# Language GADTs #-}
module Syntax where

import Data.List
import Text.PrettyPrint

import Program

{-
  What follows is a deep embedding of GRIN as a DSL with a patterns.
  GRIN is by Urban Boquist 1999.
-}
data GrinValue where
  Number   :: Integer -> GrinValue
  Variable :: Variable -> GrinValue
  Empty    :: GrinValue
  Node     :: String -> [GrinValue] -> GrinValue

data Binding a b where
  Bind :: (Pattern a, Pattern b) => a -> GrinExpression b -> Binding a b

type Alternative = [Variable] -> (GrinValue, Grin GrinValue)

data GrinExpression a where
  Sequence    :: (Pattern a, Pattern b) =>
                 GrinExpression a -> Binding a b -> GrinExpression b
  Case        :: Pattern a => Variable -> [(GrinValue, GrinExpression GrinValue)] -> GrinExpression a

  Application :: Pattern a => Variable -> [GrinValue] -> GrinExpression a
  Unit        :: Pattern a => GrinValue -> GrinExpression a
  Store       :: Pattern a => GrinValue -> GrinExpression a
  Fetch       :: Pattern a => Variable  -> Maybe Offset -> GrinExpression a
  Update      :: Pattern a => Variable  -> GrinValue    -> GrinExpression a

  Switch      :: Pattern a => Variable -> [Alternative] -> GrinExpression a

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

type Grin a = Program GrinExpression a

type Offset = Integer
type Unique = Integer

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
  show (Case v as) = error "I need to pretty print methinks"
  show (Application n vs) = show n ++ " " ++ concat (intersperse " " (map show vs))
  show (Unit v)           = "unit " ++ show v
  show (Store v)          = "store " ++ show v
  show (Fetch v n)        = "fetch " ++ show v ++ maybe "" (\n' -> "[" ++ show n' ++ "]") n
  show (Update v w)       = "update " ++ show v ++ " " ++ show w

class Pretty a where
  pretty :: a -> Doc

instance Pretty Variable where
  pretty (Register n) = char 'x' <> integer n
  pretty (Name n) = text n

instance Pretty GrinValue where
  pretty (Number n)   = integer n
  pretty (Variable v) = pretty v
  pretty (Node n vs)  = error "Node"

instance Pretty (Binding a b) where
  pretty (Bind v e) = char '\\' <> pretty (fromPattern v) <+> text "->" $$ pretty e

instance Pretty (GrinExpression a) where
  pretty (Sequence e b)     = pretty e <> semi <+> pretty b
  pretty (Case v as)        = error "Case"
  pretty (Application n vs) = undefined
  pretty (Unit v)           = text "unit" <+> pretty v
  pretty (Store v)          = text "store" <+> pretty v
  pretty (Fetch v n)        = error "Fetch"
  pretty (Update v w)       = error "Update"

pp = putStrLn . (renderStyle (style {mode = LeftMode}))
