module SimplificationLaws where

import DataTypes
import Utils

-- Add laws here
simplificationLaws' :: [String]
simplificationLaws'
  = [ "Identity: x + 0 = x"
    , "Identity: 0 + x = x"
    , "Identity: x * 1 = x"
    , "Identity: 1 * x = x"
    , "Multiplication by 0: x * 0 = 0"
    , "Multiplication by 0: 0 * x = 0"
    , "Trigonometric Identity: sin(x)^2 + cos(x)^2 = 1"
    , "Tangent definition: sin(x) / cos(x) = tan(x)"
    ]

simplificationLaws :: [Law]
simplificationLaws = generateLaws simplificationLaws'

-- sample expressions
expr1 :: Expression
expr1 = Deriv (Var "x") (Sum (Var "x") (Sum (Expt (Var "x") (Const 2)) (Const 7)))
