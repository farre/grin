module Unique where

import Control.Monad.ST.Lazy hiding (unsafeInterleaveST)
import Control.Monad.ST.Lazy.Unsafe
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

splits :: Supply s a -> [Supply s a]
splits (Supply _ s1 s2)  = s1 : splits s2

split :: Supply s a -> Supply s a
split (Supply _ s1 s2)  = s1

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
