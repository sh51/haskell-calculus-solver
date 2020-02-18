module DataTypes where

data Expression
  = Var String
  | Int Int
  | Negation Expression
  | Sum      Expression Expression
  | Sub      Expression Expression
  | Product  Expression Expression
  | Division Expression Expression
  | Expt     Expression Expression
  | Log      Expression Expression
  | Func     String Expression
  deriving (Eq, Show)

data Derivation = Derivation String Expression

-- Credit to Professor Sebastiaan Joosten. For use by calculus solver and
-- arithmetic solver.
data Calculation = Calc Expression [Step]
data Step = Step LawName Expression deriving (Show)
data Law = Law String Expression Expression deriving (Show)
