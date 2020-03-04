module Solver where

import DataTypes
-- import DerivativeLaws
import SimplificationLaws
import Utils
{-
calculate :: Expression -> Calculation
calculate e = let derivation = (derive e)
              in Calculation e (derivation ++ (simplify e derivation))
-}

simplify :: Expression -> [Step]
simplify e = rws simplificationLaws e 

calculate :: Expression -> Calculation
calculate e = Calculation e (simplify e)


simp :: Expression
simp = (Deriv (Var "x") (Expt (Var "y") (Var "x")))
