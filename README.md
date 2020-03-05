# haskell-calculus-solver

Authors: Sihao Huang, Spencer Mitchell

A calculus solver written in Haskell.

Currently: Chain rule and Exponential/Power rule are still not implemented.

To add new rules, update derivationLaws (in DerivationLaws.hs) and simplificationLaws (in SimplificationLaws.hs).

To see the sample output, run `stack ghci`, then execute `test simp`.

To test the project, run `stack ghci`, and execute `test STRING` to see the calculation for an expression STRING.





```bash
$ stack ghci
*Main DataTypes DerivativeLaws Parser Printer SimplificationLaws Solver Utils> test simp
d/dx ((5 * x) + (6 / 7))
  {Sum of derivatives}
(d/dx (5 * x) + d/dx (6 / 7))
  {Product of derivatives}
(((5 * d/dx x) + (d/dx 5 * x)) + d/dx (6 / 7))
  {Division of derivatives}
(((5 * d/dx x) + (d/dx 5 * x)) + (((d/dx 6 * 7) - (6 * d/dx 7)) / (7^2)))
  {Linear rule}
(((5 * 1) + (d/dx 5 * x)) + (((d/dx 6 * 7) - (6 * d/dx 7)) / (7^2)))
  {Constant rule}
(((5 * 1) + (0 * x)) + (((d/dx 6 * 7) - (6 * d/dx 7)) / (7^2)))
  {Constant rule}
(((5 * 1) + (0 * x)) + (((0 * 7) - (6 * d/dx 7)) / (7^2)))
  {Constant rule}
(((5 * 1) + (0 * x)) + (((0 * 7) - (6 * 0)) / (7^2)))
  {Simplification}
(5 + ((0 - 0) / (7^2)))
