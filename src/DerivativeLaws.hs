module DerivativeLaws where

import DataTypes
import Utils

derivationLaws' :: [String]
derivationLaws'
  = [ "Exponential rule: deriv(x, a ^ b) = (ln(a) * deriv(x, b)) + (deriv(x, a) * (b / a))"
    , "Sum of derivatives: deriv(x, a + b) = deriv(x, a) + deriv(x, b)"
    , "Difference of derivatives: deriv(x, a - b) = deriv(x, a) - deriv(x, b)"
    , "Product of derivatives: deriv(x, a * b) = (a * deriv(x, b)) + (deriv(x, a) * b)"
    , "Division of derivatives: deriv(x, a / b) = ((deriv(x, a) * b) - (a * (deriv(x, b)))) / (b ^ 2)"
    -- , "Chain rule: deriv(x, f(a)) = (deriv(a, f(a))) * (deriv(x, a))"
    , "Linear rule: deriv(x, x) = 1"
    , "Constant rule: deriv(x, c) = 0"
    ]

-- d/dx f(x) == d/dx f(u(x))

 -- "Constant law: deriv(x, c) = 0"
 --    , "Linear law: deriv(x, x) = 1"

derivationLaws :: [Law]
derivationLaws = generateLaws derivationLaws'

