module SimplificationLaws where

import DataTypes
import Utils

-- Simplification laws.
simplificationLaws' :: [String]
simplificationLaws'
  = [ "Identity: x + 0 = x"
    , "Identity: 0 + x = x"
    , "Identity: x * 1 = x"
    , "Identity: 1 * x = x"
    , "Multiplication by 0: x * 0 = 0"
    , "Multiplication by 0: 0 * x = 0"
    , "Multiplication Associativity: a * (b * c) = (a * b) * c"
    , "Reduction: a^b * (1/a) = a^(b-1)"
    , "Reduction: (1/a) * (a^b) = a^(b-1)"
    , "Reduction: (c*(a^b))*(1/a) = c*(a^(b-1))"
    , "Reduction: (c*(1/a))*(a^b) = c*(a^(b-1))"
    , "Trigonometric Identity: sin(x)^2 + cos(x)^2 = 1"
    , "Tangent definition: sin(x) / cos(x) = tan(x)"
    , "Subtraction by self: x - x = 0"
    , "Zero divided: 0 / x = 0"
    ]

-- The transformation of simplificationLaws' from String to Law.
simplificationLaws :: [Law]
simplificationLaws = generateLaws simplificationLaws'
