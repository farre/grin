module Core (
  module Core.Syntax,
  module Core.Value,
  transform,
) where

import Core.Value
import Core.Syntax
import qualified Syntax as Grin

transform :: Grin.Pattern a => Grin.Expression a -> Expression
transform e =
  case e of 
    Grin.Sequence e0 (Grin.Bind p e1) ->
      Sequence (transform e0) (Grin.fromPattern p) (transform e1)
    Grin.Case v alts ->
      Case v [ (p, transform e') | (p, e') <- alts ]
    Grin.Application v vs -> Application v vs
    Grin.Unit v           -> Unit v
    Grin.Store v          -> Store v
    Grin.Fetch v o        -> Fetch v o
    Grin.Update v v'      -> Update v v'