module DerivativeLaws where

import DataTypes
import Utils

derivationLaws' :: [String]
derivationLaws'
  = [--  "Power rule: deriv(x, x ^ a) = a * (x ^ (a - 1))"
    -- , "Exponential rule: deriv(x, a ^ x) = (a ^ x) * ln(a)"
     "Sum of derivatives: deriv(x, a + b) = deriv(x, a) + deriv(x, b)"
    , "Difference of derivatives: deriv(x, a - b) = deriv(x, a) - deriv(x, b)"
    , "Product of derivatives: deriv(x, a * b) = (a * deriv(x, b)) + (deriv(x, a) * b)"
    , "Division of derivatives: deriv(x, a / b) = ((deriv(x, a) * b) - (a * (deriv(x, b)))) / (b ^ 2)"
    -- , "Chain rule: deriv(x, f(a)) = (deriv(a, f(a))) * (deriv(x, a))"
    , "Sine rule: deriv(x, sin(x)) = cos(x)"
    , "Cosine rule: deriv(x, cos(x)) = -sin(x)"
    , "Natural log rule: deriv(x, ln(x)) = 1/x"
    , "Linear rule: deriv(x, x) = 1"
    , "Constant rule: deriv(x, c) = 0"
    ]

-- d/dx f(x) == d/dx f(u(x))

 -- "Constant law: deriv(x, c) = 0"
 --    , "Linear law: deriv(x, x) = 1"

derivationLaws :: [Law]
derivationLaws = generateLaws derivationLaws'

