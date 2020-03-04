module Utils where

import DataTypes

firstNonEmpty :: [[Step]] -> [Step]
firstNonEmpty [] = []
firstNonEmpty ([]:xs) = firstNonEmpty xs
firstNonEmpty (x:_) = x

shortest :: [[Step]] -> [Step]
shortest [] = []
shortest ([]:xs) = shortest xs
shortest (x:xs) = f (exprLength (expression (last x))) y
  where y = (shortest xs)
        f _ [] = x
        f a _
          | a <= (exprLength (expression (last y))) = x
          | otherwise = y

-- stepper :: ([[Step]] -> [Step]) -> [Law] -> (Expression -> [Step])
-- stepper filterFunc laws = (\e -> filterFunc (map (\f -> f e) laws))

-- matching and binding variables
-- match :: Expression -> Expression -> [Subst]
-- match (Var v) e = [(v, e)]
-- match (Sum e1 e2) (Sum e3 e4) = [m1 ++ m2 | m1 <- match e1 e3, m2 <- match e2 e4, compatible m1 m2]

-- For testing
extract :: Either a b -> b
extract (Right e) = e
extract _ = undefined
