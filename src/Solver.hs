module Solver where

import DataTypes

extract :: Either a b -> b
extract (Right x) = x
extract _ = undefined

getName :: Expression -> String
getName (Var name) = name
getName _ = undefined

-- sampleLaws :: Law
-- sampleLaws = Law "Commutativity of addiction" (Sum (Const 3) (Const 4)) (Sum (Const 4) (Const 3))

sampleLawName :: String
sampleLawName = "Commutativity of addiction"


{- Derivative Laws
Deriv x (Var y)
    | y == x = Const 1
    | otherwise = itself
Deriv x (Const _) = Const 0
Product (Const 0) _ = Const 0
Product _ (Const 0) = Const 0
Product (Const 1) expr = expr
Product expr (Const 1) = expr
Deriv x (Sum e1 e2) = Sum (Deriv x e1) (Deriv x e2)
Deriv x (Sub e1 e2) = Sub (Deriv x e1) (Deriv x e2)


-}
