module DataTypes where

data Expression
  = Var String
  | Const Int
  | Negation Expression
  | Sum      Expression Expression
  | Sub      Expression Expression
  | Product  Expression Expression
  | Division Expression Expression
  | Expt     Expression Expression
  | Func     String Expression -- Ensure first expression is (Var String)
  | Deriv    Expression Expression -- expression that needs further calculation
  deriving (Eq, Show)

-- data Derivation = Derivation Expression Expression deriving (Eq, Show) -- Expect first Expression to be (Var String)

-- For use by calculus solver and arithmetic solver.
type LawName = String
data Calculation = Calc Expression [Step]
data Step = Step LawName Expression deriving (Show)
data Law = Law String Expression Expression deriving (Show)

getName :: Expression -> String
getName (Var name) = name
getName _ = undefined
