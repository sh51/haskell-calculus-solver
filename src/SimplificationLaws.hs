module SimplificationLaws where

{-
import DataTypes
import Utils

simplificationLaws :: [Law]
simplificationLaws = [ additionRule
                     , differenceRule
                     , productRule
                     , divisionRule
                     -- , commutativeLaw
                     -- , associativeLaw
                     ]

-- Simplify the expression of the last step, if any steps exist. Otherwise
-- simplify the original expression.
simplify :: Expression -> [Step] -> [Step]
simplify _ steps@(_:_) = simplify' (expression (last steps))
simplify expr _        = simplify' expr

simplify' :: Expression -> [Step]
simplify' e = stepper firstNonEmpty simplificationLaws e

-- a + b = c
additionRule :: Law
additionRule (Sum (Const a) (Const b)) = [Step "Addition" (Const (a + b))]
additionRule (Sum a (Const 0)) = [Step "Addition" a] ++ (simplify' a)
additionRule (Sum (Const 0) b) = [Step "Addition" b] ++ (simplify' b)
additionRule (Sum a b) = addSimplifiedAB (simplify' a) (simplify' b)
  where addSimplifiedAB [] [] = []
        addSimplifiedAB sA [] = (map replaceA' sA) ++ (simplify' (Sum (expression (last sA)) b))
        addSimplifiedAB [] sB = (map (replaceB' a) sB) ++ (simplify' (Sum a (expression (last sB))))
        addSimplifiedAB sA sB = (map replaceA' sA) ++
                                (map (replaceB' (expression (last sA))) sB) ++
                                (simplify' (Sum (expression (last sA)) (expression (last sB))))
        replaceA' (Step s e) = Step s (Sum e b)
        replaceB' a' (Step s e) = Step s (Sum a' e)
additionRule _ = []

-- a - b = c
differenceRule :: Law
differenceRule (Sub (Const a) (Const b)) = [Step "Subtraction" (Const (a - b))]
differenceRule (Sub a (Const 0)) = [Step "Subtraction" a] ++ (simplify' a)
differenceRule (Sub (Const 0) b) = [Step "Subtraction" (Negation b)] ++ (simplify' (Negation b))
differenceRule (Sub a b) = subtractSimplifiedAB (simplify' a) (simplify' b)
  where subtractSimplifiedAB [] [] = []
        subtractSimplifiedAB sA [] = (map replaceA' sA) ++ (simplify' (Sub (expression (last sA)) b))
        subtractSimplifiedAB [] sB = (map (replaceB' a) sB) ++ (simplify' (Sub a (expression (last sB))))
        subtractSimplifiedAB sA sB = (map replaceA' sA) ++
                                (map (replaceB' (expression (last sA))) sB) ++
                                (simplify' (Sub (expression (last sA)) (expression (last sB))))
        replaceA' (Step s e) = Step s (Sub e b)
        replaceB' a' (Step s e) = Step s (Sub a' e)
differenceRule _ = []

-- a * b = c
productRule :: Law
productRule (Product (Const a) (Const b)) = [Step "Multiplication" (Const (a * b))]
productRule (Product _ (Const 0)) = [Step "Multiplication" (Const 0)]
productRule (Product (Const 0) _) = [Step "Multiplication" (Const 0)]
productRule (Product a (Const 1)) = [Step "Multiplication" a] ++ (simplify' a)
productRule (Product (Const 1) b) = [Step "Multiplication" b] ++ (simplify' b)
productRule (Product a b) = mulSimplifiedAB (simplify' a) (simplify' b)
  where mulSimplifiedAB [] [] = []
        mulSimplifiedAB sA [] = (map replaceA' sA) ++ (simplify' (Product (expression (last sA)) b))
        mulSimplifiedAB [] sB = (map (replaceB' a) sB) ++ (simplify' (Product a (expression (last sB))))
        mulSimplifiedAB sA sB = (map replaceA' sA) ++
                                (map (replaceB' (expression (last sA))) sB) ++
                                (simplify' (Product (expression (last sA)) (expression (last sB))))
        replaceA' (Step s e) = Step s (Product e b)
        replaceB' a' (Step s e) = Step s (Product a' e)
productRule _ = []

-- a / b = c
divisionRule :: Law
divisionRule (Division _ (Const 0)) = [Step "Division by Zero!" undefined]
divisionRule (Division (Const a) (Const b)) = [Step "Division" (Const (a `div` b))]
divisionRule (Division (Const 0) _) = [Step "Division" (Const 0)]
divisionRule (Division a (Const 1)) = [Step "Division" a] ++ (simplify' a)
divisionRule (Division a b) = divideSimplifiedAB (simplify' a) (simplify' b)
  where divideSimplifiedAB [] [] = []
        divideSimplifiedAB sA [] = (map replaceA' sA) ++ (simplify' (Division (expression (last sA)) b))
        divideSimplifiedAB [] sB = (map (replaceB' a) sB) ++ (simplify' (Division a (expression (last sB))))
        divideSimplifiedAB sA sB = (map replaceA' sA) ++
                                   (map (replaceB' (expression (last sA))) sB) ++
                                   (simplify' (Division (expression (last sA)) (expression (last sB))))
        replaceA' (Step s e) = Step s (Division e b)
        replaceB' a' (Step s e) = Step s (Division a' e)
divisionRule _ = []

-- a + b = b + c
commutativeLaw :: Law
commutativeLaw (Sum a b) = nonRedundantStep (Step "Commutative Law" (Sum b a)) (simplify' (Sum b a))
commutativeLaw (Product a b) = nonRedundantStep (Step "Commutative Law" (Product b a)) (simplify' (Product b a))
commutativeLaw _ = []


-- (a + b) + c = a + (b + c)
associativeLaw :: Law
associativeLaw (Sum (Sum a b) c) = nonRedundantStep (Step "Associative Law" (Sum a (Sum b c))) (simplify' (Sum a (Sum b c)))
associativeLaw (Product (Product a b) c) = nonRedundantStep (Step "Associative Law" (Product a (Product b c))) (simplify' (Product a (Product b c)))
associativeLaw _ = []

nonRedundantStep :: Step -> [Step] -> [Step]
nonRedundantStep unique rest
  | elem unique rest = []
  | otherwise        = unique:rest

-}
