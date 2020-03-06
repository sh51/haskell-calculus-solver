module Main where

import System.IO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Text.Megaparsec

-- import DataTypes
import DerivativeLaws
import Parser
import Printer
import SimplificationLaws
import Solver
import Utils

test :: String -> Doc ann
test = prettyCalculation.calculate.parExpr

main :: IO ()
main = do
  -- Output instructions, syntax, and laws.
  mapM_ putStrLn ([ "Enter your calculus homework problem here and I'll solve it for you."
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
                  , "\t\tAnonymous functions:\tf(a)"
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
      (Right e) -> let pprint = pretty (calculate e)
                   in do putDoc pprint
                         putStrLn "Your solution is saved in your-answer.md"
                         withFile "your-answer.md" WriteMode (\h -> do 
                                                                 hPutStrLn  h "# Your homework problem is solved!\n\n```"
                                                                 hPutDoc h pprint
                                                                 hPutStrLn h "```")
      -- Print the errors.
      (Left e) -> putStrLn (show e))
