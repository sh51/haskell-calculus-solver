module Utils where

import Data.List
import Data.Char
import Text.Megaparsec (parse)

import DataTypes
import Parser

firstNonEmpty :: [[Step]] -> [Step]
firstNonEmpty [] = []
firstNonEmpty ([]:xs) = firstNonEmpty xs
firstNonEmpty (x:_) = x

{-
shortest :: [[Step]] -> [Step]
shortest [] = []
shortest ([]:xs) = shortest xs
shortest (x:xs) = f (exprLength (expression (last x))) y
  where y = (shortest xs)
        f _ [] = x
        f a _
          | a <= (exprLength (expression (last y))) = x
          | otherwise = y

stepper :: ([[Step]] -> [Step]) -> [Law] -> (Expression -> [Step])
stepper filterFunc laws = (\e -> filterFunc (map (\f -> f e) laws))
-}

-- For testing
extract :: Either a b -> b
extract (Right e) = e
extract _ = undefined

-- match healper
mhp :: Expression -> Expression -> Expression -> Expression -> [Subst]
mhp e1 e2 e3 e4 = (nub.concat) [filterNums m1 m2| m1 <- match e1 e3, m2 <- match e2 e4, compatible m1 m2]
  where filterNums (x, a) (y, b)
          | isNum x && isNum y = []
          | isNum x = [(y, b)]
          | isNum y = [(x, a)]
          | otherwise = [(x, a), (y, b)]

-- matching and binding variables
match :: Expression -> Expression -> [Subst]
match (Var v) e = [(v, e)]
match (Const a) (Const b)
  | a == b = [(show a, Const b)]
  | otherwise = []
match (Negation e1) (Negation e2) =  match e1 e2
match (Sum e1 e2) (Sum e3 e4) = mhp e1 e2 e3 e4
match (Sub e1 e2) (Sub e3 e4) = mhp e1 e2 e3 e4
match (Product e1 e2) (Product e3 e4) = mhp e1 e2 e3 e4
match (Division e1 e2) (Division e3 e4) = mhp e1 e2 e3 e4
match (Expt e1 e2) (Expt e3 e4) = mhp e1 e2 e3 e4
-- match (Func e1 e2) (Func e3 e4) = mhp e1 e2 e3 e4
match (Func e1 e2) (Func e3 e4)
  | e1 == e3 = match e2 e4
  | otherwise = []
match (Deriv e1 e2) (Deriv e3 e4) = mhp e1 e2 e3 e4
match _ _ = []

isNum :: String -> Bool
isNum = all isDigit

-- apply :: Subst -> Expression -> Expression
-- apply sub@(v, _) e
--   | isNum v = e
--   | otherwise = applyVar sub e

apply :: Subst -> Expression -> Expression
apply (x, s) a@(Var y)
  -- | isNum x = 
  | x == y = s
  | otherwise = a
apply (x, s) a@(Const y)
  | x == show y = s
  | otherwise = a
apply sub (Negation e1) = Negation (apply sub e1)
apply sub (Sum e1 e2) = Sum (apply sub e1) (apply sub e2)
apply sub (Sub e1 e2) = Sub (apply sub e1) (apply sub e2)
apply sub (Product e1 e2) = Product (apply sub e1) (apply sub e2)
apply sub (Division e1 e2) = Division (apply sub e1) (apply sub e2)
apply sub (Expt e1 e2) = Expt (apply sub e1) (apply sub e2)
-- apply sub (Func e1 e2) = Product (apply sub e1) (apply sub e2)
apply sub (Func e1 e2) = Func e1 (apply sub e2)
apply sub (Deriv e1 e2) = Deriv e1 (apply sub e2)

compatible :: Subst -> Subst -> Bool
compatible (v1, s1) (v2, s2)
  | v1 == v2 = s1 == s2
  | otherwise = True

patMTop :: Law -> Expression -> [Expression]
patMTop (Law _ le1 le2) e3 = if null res then [] else [foldr apply le2 (match le1 e3)]
                             where res = match le1 e3

rws :: [Law] -> Expression -> [Step]
rws ls e = (concat.map (\l ->map (\e' -> (Step (lname l) e')) (rwsOne l e))) ls

-- filter out duplicates before fed into rws
rwsOne :: Law -> Expression -> [Expression]
rwsOne l e
  = patMTop l e ++
    case e of
      (Var _) -> []
      (Const _) -> []
      (Negation e') -> [Negation e'' | e'' <- rwsOne l e']
      (Sum e1 e2) -> [Sum e1' e2 | e1' <- rwsOne l e1] ++ [Sum e1 e2' | e2' <- rwsOne l e2]
      (Sub e1 e2) -> [Sub e1' e2 | e1' <- rwsOne l e1] ++ [Sub e1 e2' | e2' <- rwsOne l e2]
      (Product e1 e2) -> [Product e1' e2 | e1' <- rwsOne l e1] ++ [Product e1 e2' | e2' <- rwsOne l e2]
      (Division e1 e2) -> [Division e1' e2 | e1' <- rwsOne l e1] ++ [Division e1 e2' | e2' <- rwsOne l e2]
      (Expt e1 e2) -> [Expt e1' e2 | e1' <- rwsOne l e1] ++ [Expt e1 e2' | e2' <- rwsOne l e2]
      (Func e1 e2) -> [Func e1' e2 | e1' <- rwsOne l e1] ++ [Func e1 e2' | e2' <- rwsOne l e2]
      (Deriv e1 e2) -> [Deriv e1' e2 | e1' <- rwsOne l e1] ++ [Deriv e1 e2' | e2' <- rwsOne l e2]

generateLaws :: [String] -> [Law]
generateLaws = map (extract . (parse pLaw ""))

parExpr :: String -> Expression
parExpr s = extract (parse pExpression "" s)
leftLaw :: Law -> Expression
leftLaw (Law _ e1 _) = e1
rightLaw :: Law -> Expression
rightLaw (Law _ _ e2) = e2
