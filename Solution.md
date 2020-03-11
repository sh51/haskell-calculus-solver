# 1
The data-types can be found in DataTypes.hs.

# 2
First off all Step and Calculation are both functors, fmapping a function to a Step applies that function to the wrapped expression fmapping a function to a Calculation basically applys it to all expressions in th calculation.

pure indicates the default context for Calculation is an empty list (of Step).

The result of the sequential action, or star, is a Calculation with its first field being the base function f_start applied to base value v_start. The list of Step is the concatenation of:
 - the result of applying each function in f_steps to base value v_start
 - the result of the last function, either the tail of f_steps or f_start (in case f_steps is empty) applied to v_steps

For it to work, the parameters are expected to have the type:
 - f_start :: Expression -> Expression\\
 - f_steps :: [Step (Expression -> Expression)]\\
 - v_start :: Expression\\
 - v_steps :: [Step Expression]\\

As hinted, this applicative will come in handy when trying to combine Calculations, or add new terms to existing calculations. It would be something similar to `Sum <$> a <*> b` where assuming Sum is an expression constructor and a, b are calculations. This combines the two base expressions and append the steps from b to a. Other than that, it might also be useful in the scenario where we have a function that takes care of arithmetics/simplification and do not care about the intermediate steps. Then we can write `pure f <*> c` to simplify the steps.

# 3 4
The applicative is integrated to change the reasoning strategy. The details can be found in Solver.hs, specifically, function `calculate'`.
This update doesn't add new features, to test the implementation, execute `testSolver` in ghci. Run the command-line tool/Call main to test other expressions.