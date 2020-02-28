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

data Calculation = Calculation Expression [Step] deriving (Show)
data Step = Step { lawName    :: String
                 , expression :: Expression
                 } deriving (Show)

type Law = Expression -> [Step]
laws :: [Law]
laws = [sumRule, subRule, constantRule]

firstNonEmpty :: [[a]] -> [a]
firstNonEmpty [] = []
firstNonEmpty ([]:xs) = firstNonEmpty xs
firstNonEmpty (x:_) = x

calculate :: Expression -> Calculation
calculate e = Calculation e (derive e)

derive :: Expression -> [Step]
derive e = firstNonEmpty (map (\f -> f e) laws)

constantRule :: Law
constantRule (Deriv _ (Const _)) = [Step "Constant Rule" (Const 0)]
constantRule _                   = []

sumRule :: Law
sumRule (Deriv v (Sum a b))
  = f (derive (Deriv v a)) (derive (Deriv v b))
  where f [] _    = []
        f _ []    = []
        f lhs rhs = (Step "Sum Rule" (Sum (Deriv v a) (Deriv v b))):((map lhsF lhs) ++ (map rhsF rhs))
          where lhsF (Step s e) = Step s (Sum e (Deriv v b))
                rhsF (Step s e) = Step s (Sum lastLhs e)
                lastLhs = expression (last lhs)
sumRule _ = []

subRule :: Law
subRule (Deriv v (Sub a b))
  = f (derive (Deriv v a)) (derive (Deriv v b))
  where f [] _    = []
        f _ []    = []
        f lhs rhs = (Step "Difference Rule" (Sub (Deriv v a) (Deriv v b))):((map lhsF lhs) ++ (map rhsF rhs))
          where lhsF (Step s e) = Step s (Sub e (Deriv v b))
                rhsF (Step s e) = Step s (Sub lastLhs e)
                lastLhs = expression (last lhs)
subRule _ = []

simp, simp2, simp3 :: Expression
simp = (Deriv (Var "x") (Const 4))
simp2 = (Deriv (Var "x") (Sum (Const 4) (Const 3)))
simp3 = (Deriv (Var "x") (Sum (Const 4) (Sub (Const 2) (Const 3))))

getName :: Expression -> String
getName (Var name) = name
getName _ = undefined
