module Utils where

import Data.List
import Data.Char
import DataTypes

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
mhp e1 e2 e3 e4 = (nub.concat) [[m1,m2]| m1 <- match e1 e3, m2 <- match e2 e4, compatible m1 m2]
-- matching and binding variables
match :: Expression -> Expression -> [Subst]
match (Var v) e = [(v, e)]
match (Const a) (Const b)
  | a == b = [(show a, Const b)]
  | otherwise = []
match (Sum e1 e2) (Sum e3 e4) = mhp e1 e2 e3 e4
match (Sub e1 e2) (Sub e3 e4) = mhp e1 e2 e3 e4
match (Func name1 e1) (Func name2 e2)
  | name1 == name2 = match e1 e2
  | otherwise = []
-- TD

apply :: Subst -> Expression -> Expression
apply sub@(v, s) e
  | isDigit (head v) = e
  | otherwise = case e of
      (Var a)
        | v == a -> s
        | otherwise -> Var a
      (Product e1 e2) -> Product (apply sub e1) (apply sub e2)
  -- TD

compatible :: Subst -> Subst -> Bool
compatible (v1, s1) (v2, s2)
  | v1 == v2 = s1 == s2
  | otherwise = True

-- patMTop :: Law -> Expression -> [Step]
-- patMTop (Law lawname le1 le2) e3 = map (\e -> Step lawname e) [apply sub le2
--                                                      | sub <- match le1 e3]
patMTop :: Law -> Expression -> [Expression]
patMTop (Law lawname le1 le2) e3 = [apply sub le2 | sub <- match le1 e3]
-- rwsHelper :: Expression -> Expression -> Expression -> Expression -> Expression -> [Step]
-- rwsHelper f = 

rws :: [Law] -> Expression -> [Step]
rws ls e = (concat.map (\l ->map (Step (lname l)) (rwsOne l e))) ls
  
rwsOne :: Law -> Expression -> [Expression]
-- rwsOne ls e = map (\e -> patMTop l e) ls ++ case e of
rwsOne l e = patMTop l e ++ case e of
  (Sum e1 e2) -> [Sum e1' e2 | e1' <- rwsOne l e1] ++ [Sum e1 e2' | e2' <- rwsOne l e2]
-- TD
  

