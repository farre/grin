module Pretty (pp) where

import Syntax

import Text.PrettyPrint

class Pretty a where
  pretty :: a -> Doc

instance Pretty Variable where
  pretty (Register n) = char 'x' <> integer n
  pretty (Name n) = text n

instance Pretty GrinValue where
  pretty (Number n)   = integer n
  pretty (Variable v) = pretty v
  pretty (Node n vs)  = parens $ text n <+> hsep (vs >>= return . pretty)

instance Pretty (GrinExpression a) where
  pretty (Sequence e0 (Bind v e1)) =
    (pretty e0 <> semi <+>
     char '\\' <> pretty (fromPattern v) <+> text "->") $+$ pretty e1
  pretty (Case v as) =
    lparen <> hang (text "case" <+> pretty v <+> text "of")
    4 (vcat $ [ pretty v <+> text "->" <+> pretty e | (v, e) <- as]) $$ rparen
  pretty (Application n vs) = pretty n <+> hsep (vs >>= return . pretty)
  pretty (Unit v)           = text "unit" <+> pretty v
  pretty (Store v)          = text "store" <+> pretty v
  pretty (Fetch v n)        = text "fetch" <+> pretty v <+>
                              maybe empty (brackets . integer) n
  pretty (Update v w)       = text "update" <+> pretty v <+> pretty w


pp :: Pretty a => a -> IO ()
pp = putStrLn . (renderStyle style) . pretty
