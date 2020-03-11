module Solver where

import DataTypes
import Utils
{-
-- Initial implementation
-- Simplify the expression.
simplify' :: [Law] -> Expression -> [Step Expression]
simplify' ls e = f steps
  where steps = rws ls e
        f [] = []
        f (x:_) = x:(simplify' ls (expression x))

-- Wrapper around simplify'.
-- Adds the "Simplification" step.
simplify :: Bool -> [Law] -> Expression -> [Step Expression]
simplify verbose ls e = f (simplify' ls e)
  where f [] = []
        f e' = if verbose
               then e'
               else [Step "Simplification" (expression $ last e')]

-- Derive the expression.
derive :: [Law] -> Expression -> [Step Expression]
derive ls e = f steps
  where steps = rws ls e
        f [] = []
        f (x:_) = x:(derive ls (expression x))

-- Calculate the expression from beginning to end.
calculate :: [Law] -> [Law] -> Expression -> Bool -> Calculation Expression
calculate dLaws sLaws e verbose = Calculation e (derivation ++ (simplify verbose sLaws (f e derivation)))
  where derivation = (derive dLaws e)
        -- Select the expression from the last step if it exists,
        -- otherwise return e'.
        f e' [] = e'
        f _ steps = expression (last steps)

-}

calculate' :: [Law] -> Expression -> Calculation Expression
calculate' ls e  = case e of
  {-
                    Const c -> reasonTop ls (pure (Const c))
                    Var x -> reasonTop ls (pure (Var x))
                    (Negation e') -> reasonTop ls (pure (Negation e'))-}
             
                    (Sum e1 e2) -> reasonTop ls (Sum <$> calculate' ls e1 <*> calculate' ls e2)
                    (Sub e1 e2) -> Sub <$> calculate' ls e1 <*> calculate' ls e2
                    (Product e1 e2) -> Product <$> calculate' ls e1 <*> calculate' ls e2
                    (Division e1 e2) -> Division <$> calculate' ls e1 <*> calculate' ls e2
                    (Expt e1 e2) -> Expt <$> calculate' ls e1 <*> calculate' ls e2
                    e' -> reasonTop ls (pure e')

calculate :: [Law] -> [Law] -> Expression -> Bool -> Calculation Expression
calculate dLaws sLaws e verbose = combcal dresult (if verbose then sresult else Calculation (calBase sresult) [last (calSteps sresult)])
  where dresult = (calculate' dLaws e)
        sresult = (calculate' sLaws lastexpr)
        lastexpr = case (calSteps dresult) of
          [] -> e
          sts -> expression $ last sts
        combcal (Calculation e1 sts1) (Calculation _ sts2)
          = Calculation e1 (sts1 ++ sts2)

{-
reason' :: Bool -> [Law] -> Expression -> Calculation Expression
reason' verbose ls e = case verbose of
                         True -> reason ls e
                         _ -> Calculation e [last (calSteps (reason ls e))]
-}

reason :: [Law] -> Expression ->[Step Expression]
reason ls e = f steps
  where steps = rws ls e
        f [] = []
        f (x:_) = x:(reason ls (expression x))


-- do the simplification from top level again, in case combining the calculations generates new replacable patterns
reasonTop :: [Law] -> Calculation Expression -> Calculation Expression
reasonTop ls (Calculation e sts) = Calculation e (sts ++ (reason ls (case sts of
                                                   [] -> e
                                                   sts' -> (expression.last) sts')))




