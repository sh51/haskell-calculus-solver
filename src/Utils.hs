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

stepper :: ([[Step]] -> [Step]) -> [Law] -> (Expression -> [Step])
stepper filterFunc laws = (\e -> filterFunc (map (\f -> f e) laws))

-- For testing
extract :: Either a b -> b
extract (Right e) = e
extract _ = undefined
