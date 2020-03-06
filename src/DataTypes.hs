module DataTypes where

-- The Expression data type is the core of our representation of expressions.
data Expression
  = Var String
  | Const Int
  | Negation Expression
  | Sum      Expression Expression
  | Sub      Expression Expression
  | Product  Expression Expression
  | Division Expression Expression
  | Expt     Expression Expression
  | Func     Expression Expression -- Ensure first expression is (Var String)
  | Deriv    Expression Expression -- expression that needs further calculation
  deriving (Eq, Show)

-- A Calculation contains the original expression and all steps to solve that
-- expression.
data Calculation = Calculation Expression [Step] deriving (Show)

-- A Step contains the resulting expression for a given law.
data Step = Step { lawName    :: String
                 , expression :: Expression
                 } deriving (Eq, Show)

-- A Law contains a rule, an expression, and the transformed expression
-- associated with the rule.
data Law = Law {lname :: String, expr1 :: Expression, expr2 :: Expression} deriving (Eq, Show)

-- A Subst contains a string representation of a symbol and the Expression
-- that can be substituted for that symbol.
type Subst = (String, Expression)

-- Get the underlying string of a Var constructor.
getName :: Expression -> String
getName (Var name) = name
getName _ = undefined
