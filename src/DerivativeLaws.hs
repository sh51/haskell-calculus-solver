module DerivativeLaws where

import DataTypes
import Utils

derivationLaws' :: [String]
derivationLaws'
  = [--  "Power rule: deriv(x, x ^ a) = a * (x ^ (a - 1))"
    -- , "Exponential rule: deriv(x, a ^ x) = (a ^ x) * ln(a)"
      "Exponential rule: deriv(x, a ^ b) = a^b * deriv(x, ln(a)*b)"
    , "Sum of derivatives: deriv(x, a + b) = deriv(x, a) + deriv(x, b)"
    , "Difference of derivatives: deriv(x, a - b) = deriv(x, a) - deriv(x, b)"
    , "Product of derivatives: deriv(x, a * b) = (a * deriv(x, b)) + (deriv(x, a) * b)"
    , "Division of derivatives: deriv(x, a / b) = ((deriv(x, a) * b) - (a * (deriv(x, b)))) / (b ^ 2)"
    -- , "Chain rule: deriv(x, f(a)) = (deriv(a, f(a))) * (deriv(x, a))"
    , "Sine rule: deriv(x, sin(a)) = cos(a)*deriv(x, a)"
    , "Cosine rule: deriv(x, cos(a)) = -sin(a)*deriv(x, a)"
    , "Tangent rule: deriv(x, tan(a)) = deriv(x,a)*1/(cos(a)^2))"
    , "Natural log rule: deriv(x, ln(a)) = deriv(x, a)*1/a"
    , "Linear rule: deriv(x, x) = 1"
    , "Constant rule: deriv(x, c) = 0"
    ]

-- d/dx f(x) == d/dx f(u(x))

 -- "Constant law: deriv(x, c) = 0"
 --    , "Linear law: deriv(x, x) = 1"

derivationLaws :: [Law]
derivationLaws = generateLaws derivationLaws'

