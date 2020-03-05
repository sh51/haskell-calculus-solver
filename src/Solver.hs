module Solver where

import DataTypes
import DerivativeLaws
import SimplificationLaws
import Utils

simplify' :: Expression -> [Step]
simplify' e = f steps
  where steps = rws simplificationLaws e
        f [] = []
        f (x:_) = x:(simplify' (expression x))

simplify :: Expression -> [Step]
simplify e = f (simplify' e)
  where f [] = []
        f e' = [Step "Simplification" (expression $ last e')]

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


-- simp :: Expression
-- simp = Deriv (Var "x") (Sum (Product (Const 5) (Var "x")) (Division (Const 6) (Const 7)))
simp :: String
simp = "deriv(x, 5*x + 6/7)"
