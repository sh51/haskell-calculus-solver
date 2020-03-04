module DerivativeLaws where
{-
import DataTypes
import Utils

derivativeLaws :: [Law]
derivativeLaws = [ naturalLogRule
                 , logRule
                 , powerRule
                 , exponentialRule
                 , sumRule
                 , differenceRule
                 , productRule
                 , quotientRule
                 , chainRule
                 , linearRule
                 , constantRule
                 , fallBackRule
                 ]

derive :: Expression -> [Step]
derive e = stepper firstNonEmpty derivativeLaws e

-- d/dx a = 0
constantRule :: Law
constantRule (Deriv _ (Const _)) = [Step "Constant Rule" (Const 0)]
constantRule (Deriv x (Var y))
  | getName x /= y               = [Step "Constant Rule" (Const 0)]
  | otherwise                    = []
constantRule _                   = []

-- d/dx x = 1
linearRule :: Law
linearRule expr@(Deriv x (Var y))
  | getName x == y    = [Step "Linear Rule" (Const 1)]
  | otherwise         = constantRule expr
linearRule _          = []

-- d/dx a ^ b = (a ^ b) * (b' * ln(a) + a' * (b / a))
exponentialRule :: Law
exponentialRule (Deriv x expt@(Expt a b))
  = f (derive (Deriv x b)) (derive (Deriv x a))
  where f [] _    = []
        f _ []    = []
        f lhs rhs = [Step "Exponential Rule" (Product
                                              expt
                                              (Sum
                                               (Product (Deriv x b) (Func "ln" a))
                                               (Product (Deriv x a) (Division b a))))] ++
            (map replaceB' lhs) ++
            (map replaceA' rhs)
          where lastLhs = expression (last lhs)
                replaceB' (Step s e) = Step s (Product
                                               expt
                                               (Sum
                                                (Product e (Func "ln" a))
                                                (Product (Deriv x a) (Division b a))))
                replaceA' (Step s e) = Step s (Product
                                               expt
                                               (Sum
                                                (Product lastLhs (Func "ln" a))
                                                (Product e (Division b a))))
  
exponentialRule _  = []

-- d/dx ln(x) = 1 / x
naturalLogRule :: Law
naturalLogRule (Deriv x (Func "ln" var@(Var y)))
  | getName x == y = [Step "Natural Log Rule" (Division (Const 1) x)]
  | otherwise      = constantRule (Deriv x var)
naturalLogRule _   = []

-- d/dx log(x) = 1 / (x * ln(10))  -- assuming log == log_10
logRule :: Law
logRule (Deriv x (Func "log" var@(Var y)))
  | getName x == y    = [Step "Log Rule" (Division (Const 1) (Product x (Func "ln" (Const 10))))]
  | otherwise = constantRule (Deriv x var)
logRule _     = []

-- constantMultiplicationRule :: Law
-- constantMultiplicationRule (Deriv x (Product c@(Const _) expr))
--   = [Step "Multiplication by Constant Rule" (Product c (Deriv x expr))] ++ (map replaceC steps)
--   where steps               = derive (Deriv x expr)
--         replaceC (Step s e) = Step s (Product c e)
-- constantMultiplicationRule _ = []

-- d/dx x^a = a * (x ^ (a - 1))
powerRule :: Law
powerRule (Deriv x (Expt var@(Var y) expr))
  | getName x == y = [Step "Power Rule" (Product expr (Expt var (Sub expr (Const 1))))]
  | otherwise      = []
powerRule _        = []

-- d/dx a + b = a' + b'
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

-- d/dx a - b = a' - b'
differenceRule :: Law
differenceRule (Deriv v (Sub a b))
  = f (derive (Deriv v a)) (derive (Deriv v b))
  where f [] _    = []
        f _ []    = []
        f lhs rhs = (Step "Difference Rule" (Sub (Deriv v a) (Deriv v b))):((map lhsF lhs) ++ (map rhsF rhs))
          where lhsF (Step s e) = Step s (Sub e (Deriv v b))
                rhsF (Step s e) = Step s (Sub lastLhs e)
                lastLhs = expression (last lhs)
differenceRule _ = []

-- d/dx a * b = a * b' + a' * b
productRule :: Law
productRule (Deriv x (Product a b))
  = f (derive (Deriv x b)) (derive (Deriv x a))
  where f [] _    = []
        f _ []    = []
        f lhs rhs = [Step "Product Rule" (Sum (Product a (Deriv x b)) (Product (Deriv x a) b))] ++
                    (map replaceB' lhs) ++
                    (map replaceA' rhs)
          where lastLhs = expression (last lhs)
                replaceB' (Step s e) = Step s (Sum (Product a e) (Product (Deriv x a) b))
                replaceA' (Step s e) = Step s (Sum (Product a lastLhs) (Product e b))
productRule _ = []

-- d/dx a / b = (a’ * b − b’ * a) / b ^ 2
quotientRule :: Law
quotientRule (Deriv x (Division a b))
  = f (derive (Deriv x a)) (derive (Deriv x b))
  where f [] _    = []
        f _ []    = []
        f lhs rhs = [Step "Quotient Rule" (Division
                                           (Sub
                                            (Product (Deriv x a) b)
                                            (Product a (Deriv x b)))
                                           (Expt b (Const 2)))] ++
                    (map replaceA' lhs) ++
                    (map replaceB' rhs)
          where lastLhs = expression (last lhs)
                replaceA' (Step s e) = Step s (Division
                                               (Sub
                                                (Product e b)
                                                (Product a (Deriv x b)))
                                               (Expt b (Const 2)))
                replaceB' (Step s e) = Step s (Division
                                               (Sub
                                                (Product lastLhs b)
                                                (Product a e))
                                               (Expt b (Const 2)))
quotientRule _ = []

-- d/dx f(g) = f’(g) * g’
chainRule :: Law
chainRule (Deriv x func@(Func _ expr))
  = f (derive (Deriv x expr))
  where f steps                   = [Step "Chain Rule" (Product derivedFunc (Deriv x expr))] ++
                                    (map replaceExpr' steps)
        replaceExpr' (Step s e)   = Step s (Product derivedFunc e)
        derivedFunc               = deriveFunc func
        deriveFunc (Func "sin" e) = Func "cos" e
        deriveFunc (Func "cos" e) = Negation (Func "sin" e)
        deriveFunc (Func "tan" e) = Expt (Func "sec" e) (Const 2)
        deriveFunc (Func a e)     = Func (a ++ "'") e
        deriveFunc _              = undefined
        
chainRule _ = []

fallBackRule :: Law
fallBackRule (Deriv _ expr) = [Step "Prime Rule" (Prime expr)]
fallBackRule _              = []
-}
