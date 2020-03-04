module Solver where

import DataTypes
import DerivativeLaws
import SimplificationLaws
{-
calculate :: Expression -> Calculation
calculate e = let derivation = (derive e)
              in Calculation e (derivation ++ (simplify e derivation))
-}


simp :: Expression
simp = (Deriv (Var "x") (Expt (Var "y") (Var "x")))
