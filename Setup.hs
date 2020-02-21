import Distribution.Simple
import Parser
import Solver

main = do
	putStrLn "Derivative Laws:"
	putStrLn sampleLaws
	expr <- pExpression
	putStrLn "Derive" ++ expr
	putStrLn "= " ++ sampleLawName
	putStrLn expr
