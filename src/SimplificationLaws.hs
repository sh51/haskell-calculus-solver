module SimplificationLaws where

import DataTypes
import Utils

-- Add laws here
simplificationLaws' :: [String]
simplificationLaws'
  = [ "Simplification Law 0: x + 0 = x"
    , "Simplification Law 1: 0 + x = x"
    , "Simplification Law 2: 0 * x = 0"
    ]

simplificationLaws :: [Law]
simplificationLaws = generateLaws simplificationLaws'

-- sample expressions
expr1 :: Expression
expr1 = Deriv (Var "x") (Sum (Var "x") (Sum (Expt (Var "x") (Const 2)) (Const 7)))
