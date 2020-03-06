module Solver where

import DataTypes
import DerivativeLaws
import SimplificationLaws
import Utils

-- Simplify the expression.
simplify' :: Expression -> [Step]
simplify' e = f steps
  where steps = rws simplificationLaws e
        f [] = []
        f (x:_) = x:(simplify' (expression x))

-- Wrapper around simplify'.
-- Adds the "Simplification" step.
simplify :: Expression -> [Step]
simplify e = f (simplify' e)
  where f [] = []
        f e' = [Step "Simplification" (expression $ last e')]

-- Derive the expression.
derive :: Expression -> [Step]
derive e = f steps
  where steps = rws derivationLaws e
        f [] = []
        f (x:_) = x:(derive (expression x))

-- Calculate the expression from beginning to end.
calculate :: Expression -> Calculation
calculate e = Calculation e (derivation ++ (simplify (f e derivation)))
  where derivation = (derive e)
        -- Select the expression from the last step if it exists,
        -- otherwise return e'.
        f e' [] = e'
        f _ steps = expression (last steps)
