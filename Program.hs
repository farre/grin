{-# Language GADTs #-}
module Program where

{-
  This is basically 'operational' as seen on hackage, by (c) Heinrich
  Apfelmus 2010-2011 (BSD3), modified for my purposes.
-}
data Program instr a where
  Then   :: instr a -> (a -> Program instr b) -> Program instr b
  Return :: a -> Program instr a

instance Monad (Program instr) where
  return = Return
  x >>= f = case x of
    Return i    -> f i
    i `Then` is -> i `Then` (\a -> is a >>= f)
