module Core (
  module Core.Syntax,
  module Core.Value,
  module Core.Variable,
  transform,
) where

import Core.Value
import Core.Variable
import Core.Syntax
import qualified Syntax as Grin
import Syntax ( Pattern(..), Valueable(..) )

transform :: Grin.Expression a -> Expression
transform e =
  case e of 
    Grin.Sequence e0 (Grin.Bind p e1) ->
      Sequence (transform e0) (fromValue . fromPattern $ p) (transform e1)
    Grin.Case v alts ->
      Case v [ (fromValue p, transform e') | (p, e') <- alts ]
    Grin.Application v vs -> Application v (map fromValue vs)
    Grin.Unit v           -> Unit (fromValue v)
    Grin.Store v          -> Store (fromValue v)
    Grin.Fetch v o        -> Fetch v o
    Grin.Update v v'      -> Update v (fromValue v')