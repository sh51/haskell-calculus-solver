module Solver where

import DataTypes
import Utils

-- Simplify the expression.
simplify' :: [Law] -> Expression -> [Step]
simplify' ls e = f steps
  where steps = rws ls e
        f [] = []
        f (x:_) = x:(simplify' ls (expression x))

-- Wrapper around simplify'.
-- Adds the "Simplification" step.
simplify :: Bool -> [Law] -> Expression -> [Step]
simplify verbose ls e = f (simplify' ls e)
  where f [] = []
        f e' = if verbose
               then e'
               else [Step "Simplification" (expression $ last e')]

-- Derive the expression.
derive :: [Law] -> Expression -> [Step]
derive ls e = f steps
  where steps = rws ls e
        f [] = []
        f (x:_) = x:(derive ls (expression x))

-- Calculate the expression from beginning to end.
calculate :: [Law] -> [Law] -> Expression -> Bool -> Calculation
calculate dLaws sLaws e verbose = Calculation e (derivation ++ (simplify verbose sLaws (f e derivation)))
  where derivation = (derive dLaws e)
        -- Select the expression from the last step if it exists,
        -- otherwise return e'.
        f e' [] = e'
        f _ steps = expression (last steps)
