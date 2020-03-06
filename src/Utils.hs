module Utils where

import Data.List
import Data.Char
import Text.Megaparsec (parse)

import DataTypes
import Parser

-- For testing
extract :: Either a b -> b
extract (Right e) = e
extract _ = undefined

-- match helper
mhp :: Expression -> Expression -> Expression -> Expression -> [Subst]
mhp e1 e2 e3 e4 = nub (combMatch (match e1 e3) (match e2 e4))

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
match (Func e1 e2) (Func e3 e4)
  | e1 == e3 = match e2 e4
  | otherwise = []
match (Deriv e1 e2) (Deriv e3 e4) = mhp e1 e2 e3 e4
match _ _ = []

-- apply a substitution to an expression
apply :: Subst -> Expression -> Expression
apply (x, s) a@(Var y)
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
apply sub (Func e1 e2) = Func e1 (apply sub e2)
apply sub (Deriv e1 e2) = Deriv e1 (apply sub e2)

-- combine the matches from both sides, empty if they are incompatible
combMatch :: [Subst] -> [Subst] -> [Subst]
combMatch subs1@(_:_) subs2@(_:_)
  | compatibleAll subs1 subs2 = subs1 ++ subs2
  | otherwise = []
combMatch _ _ = []

-- ensure both substitutions are compatible with each other
compatible :: Subst -> Subst -> Bool
compatible (v1, s1) (v2, s2)
  | v1 == v2 = s1 == s2
  | otherwise = True

-- ensure all substitutions in one list are compatible with all subsitutions
-- of another list
compatibleAll :: [Subst] -> [Subst] -> Bool
compatibleAll subs1 subs2 = and [compatible sub1 sub2| sub1 <- subs1, sub2 <- subs2]

-- pattern match a top level expression, applying all substitutions
patMTop :: Law -> Expression -> [Expression]
patMTop (Law _ le1 le2) e3 = if null res then [] else [foldr apply le2 res]
  where res = filter (not.isDigit.head.fst) (match le1 e3)

-- rewrite wrapper, wrapping rewrites in steps
rws :: [Law] -> Expression -> [Step]
rws ls e = (concat.map (\l ->map (\e' -> (Step (lname l) e')) (rwsOne l e))) ls

-- recursively perform rewrites on expressions
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

-- helper function to create a list of laws from a list of strings
generateLaws :: [String] -> [Law]
generateLaws = map (extract . (parse pLaw ""))

-- helper parser to parse laws
parExpr :: String -> Expression
parExpr s = extract (parse pExpression "" s)
