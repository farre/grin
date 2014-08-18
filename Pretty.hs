module Pretty ( pp, render ) where

import Syntax

import Text.PrettyPrint hiding ( render )

class Pretty a where
  pretty :: a -> Doc

instance Pretty Name where
  pretty (Name n) = text n

instance Pretty Variable where
  pretty (Register n) = char 'x' <> integer n
  pretty (VariableName n) = pretty n

prettyList = hsep . ((return $!) . pretty =<<)

instance Pretty GrinValue where
  pretty (Number n)   = integer n
  pretty (Variable v) = pretty v
  pretty (Node t vs)  = parens $ pretty t <+> prettyList vs

instance Pretty (Expression a) where
  pretty (Sequence e0 (Bind p e1)) =
    e0' `seq` p' `seq` e1' `seq` (e0' <> semi <+>
     char '\\' <> p' <+> text "->") $+$ e1'
    where p' = pretty (fromPattern p)
          e0' = pretty e0
          e1' = pretty e1
  pretty (Case v as) =
    v' `seq` cs `seq`
      lparen <> hang (text "case" <+> pretty v <+> text "of")
    4 cs $$ rparen
    where
      v' = pretty v
      cs = vcat $ [ v' <+> text "->" <+> pretty e | (v, e) <- as]
  pretty (Application n vs) = pretty n <+> prettyList vs
  pretty (Unit v)           = text "unit" <+> pretty v
  pretty (Store v)          = text "store" <+> pretty v
  pretty (Fetch v n)        = text "fetch" <+> pretty v <+>
                              maybe empty (brackets . integer) n
  pretty (Update v w)       = text "update" <+> pretty v <+> pretty w

instance Pretty Declaration where
  pretty (Declaration n vs e) = pretty n <+> prettyList vs <+> char '=' <+> pretty e

pp :: Pretty a => a -> IO ()
pp = putStrLn . render

render :: Pretty a => a -> String
render = (renderStyle style) . pretty
