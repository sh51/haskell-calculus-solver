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
  | Func     Expression Expression -- Ensure first expression if (Var String)
  deriving (Eq, Show)

data Derivation = Derivation String Expression

-- For use by calculus solver and arithmetic solver.
type LawName = String
data Calculation = Calc Expression [Step]
data Step = Step LawName Expression deriving (Show)
data Law = Law String Expression Expression deriving (Show)
