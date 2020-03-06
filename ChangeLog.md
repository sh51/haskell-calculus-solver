# Changelog for haskell-calculus-solver
## 0.6.0.0
- Chain Rule and Exponential Rule implemented
- Added simplification of exponential terms (to some degree)
- More simplification rules
- 
## 0.5.0.0

- Law Parser: Laws are now configurable (Add laws in DerivationLaws.hs and SimplificationLaws.hs)
- Pretty Printer (formatted output)
- Added test expression simp (d/dx(5*x + 6/7))

## 0.1.2.0

- Updated Parser (now supports function calls without the '$' sign)
- Basic Reasoner (implemented sum, difference and const rules)
- Test expressions (simp, simp2, simp3) for reasoner

## 0.1.1.0

- Parser for expressions (in Parser.hs).
- Parser to handle input from user (in Parser.hs).
- Tests for parsers using hspec (in Spec.hs).
- Example output (input "deriv x x^2" after executing the program)

## Initial version (0.1.0.0)

- Initial data types (in DataTypes.hs).
