module Main where

import Parser
import Solver



main :: IO ()
-- main = return ()
main = do
	putStrLn "Derivative Laws:"
	putStrLn sampleLaws
	expr <- pExpression
	putStrLn "Derive" ++ expr
	putStrLn "= " ++ sampleLawName
	putStrLn expr

