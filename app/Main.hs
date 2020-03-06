module Main where

import System.Environment
import System.IO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Text.Megaparsec

import Parser
import Printer ()
import Solver
import Utils

main :: IO ()
main = do
  -- Check for verbose flag.
  args <- getArgs
  -- Get laws from files.
  derivationLaws' <- lawsFromFile "DerivationLaws.txt"
  simplificationLaws' <- lawsFromFile "SimplificationLaws.txt"
  let derivationLaws = generateLaws derivationLaws'
      simplificationLaws = generateLaws simplificationLaws'
      verbose = "-v" `elem` args
  -- Output instructions, syntax, and laws.
  mapM_ putStrLn ([ "Enter your calculus homework problem here and I'll solve it for you."
                  , ""
                  , "Pass the -v flag when starting the program to include printing simplification steps."
                  , ""
                  , "The following syntax is allowed:"
                  , "\tAddition:\ta + b"
                  , "\tSubtraction:\ta - b"
                  , "\tMultiplication:\ta * b"
                  , "\tDivision:\ta / b"
                  , "\tExponentiation:\ta ^ b"
                  , "\tSub-expressions in parenthesis:\ta + (b + c)"
                  , "\tFunction calls:"
                  , "\t\tSine:\tsin(a)"
                  , "\t\tCosine:\tcos(a)"
                  , "\t\tTangent:\ttan(a)"
                  , "\t\tNatural log:\tln(a)"
                  , "\t\tDerivation:\tderiv(x, x ^ 2)"
                  , "\t\tThe derivation function takes the variable to derive on as it's first"
                  , "\t\targument, and the expression to derive as the second argument."
                  , ""
                  , "The following laws and rules can be solved:"
                  ]
                  ++ (map ((:) '\t') derivationLaws')
                  ++ (map ((:) '\t') simplificationLaws')
                  ++ [""])
  -- Prompt user for input.
  putStr "> "
  hFlush stdout
  s <- getLine
  -- Parse the input.
  (case (parse pExpression "" s) of
      -- Print the solution to screen and save as markdown to file.
      (Right e) -> let pprint = pretty (calculate derivationLaws simplificationLaws e verbose)
                   in do putDoc pprint
                         putStrLn "Your solution is saved in your-answer.md"
                         withFile "your-answer.md" WriteMode (\h -> do 
                                                                 hPutStrLn  h "# Your homework problem is solved!\n\n```"
                                                                 hPutDoc h pprint
                                                                 hPutStrLn h "```")
      -- Print the errors.
      (Left e) -> putStrLn (show e))
