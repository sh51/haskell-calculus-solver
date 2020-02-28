# haskell-calculus-solver

Authors: Sihao Huang, Spencer Mitchell

A calculus solver written in Haskell.

Currently: Reasoner is still in progress.

To build the project, run `stack build`. To test the project run `stack test`.

To see parsed input from the commandline run `stack exec haskell-calculus-solver`.

To test the currently implemented rules, run `stack ghci`, then `calculate simp3` (simp3 is a sample expression)

```bash
$ stack exec haskell-calculus-solver
> deriv x x ^ 2 + 2 * x
Right (Derivation (Var "x") (Sum (Expt (Var "x") (Const 2)) (Product (Const 2) (Var "x"))))
>      # Hitting return with no input will exit the program.
$
```
