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
  | Prime    Expression        -- Fall back during derivations.
  | Deriv    Expression Expression -- expression that needs further calculation
  deriving (Eq, Show)

data Calculation = Calculation Expression [Step] deriving (Show)
data Step = Step { lawName    :: String
                 , expression :: Expression
                 } deriving (Show)
type Law = Expression -> [Step]

getName :: Expression -> String
getName (Var name) = name
getName _ = undefined
