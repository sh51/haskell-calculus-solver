module Solver where

import Control.Monad
import DataTypes
import DerivativeLaws
import SimplificationLaws
import Utils

simplify :: Expression -> [Step]
simplify e = f steps
  where steps = rws simplificationLaws e
        f [] = []
        f (x:_) = x:(simplify (expression x))

derive :: Expression -> [Step]
derive e = f steps
  where steps = rws derivationLaws e
        f [] = []
        f (x:_) = x:(derive (expression x))
-- TODO: 

-- calculate :: Expression -> Calculation
-- calculate e = Calculation e (derive e)

calculate :: Expression -> Calculation
calculate e = Calculation e (derivation ++ (simplify (f e derivation)))
  where derivation = (derive e)
        f e' [] = e'
        f _ steps = expression (last steps)


simp :: Expression
simp = (Deriv (Var "x") (Expt (Var "y") (Var "x")))
