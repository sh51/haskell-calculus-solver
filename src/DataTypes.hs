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
                 } deriving (Eq, Show)
type Law = Expression -> [Step]
data Law' = Law' String Expression Expression deriving (Show)

getName :: Expression -> String
getName (Var name) = name
getName _ = undefined

exprLength :: Expression -> Integer
exprLength (Var _) = 1
exprLength (Const _) = 1
exprLength (Negation a) = exprLength a
exprLength (Sum a b) = (exprLength a) + (exprLength b)
exprLength (Sub a b) = (exprLength a) + (exprLength b)
exprLength (Product a b) = (exprLength a) + (exprLength b)
exprLength (Division a b) = (exprLength a) + (exprLength b)
exprLength (Expt a b) = (exprLength a) + (exprLength b)
exprLength (Func _ a) = 1 + (exprLength a)
exprLength (Prime a) = exprLength a
exprLength (Deriv _ a) = exprLength a
