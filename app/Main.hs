module Main where

import System.IO

import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.Render.Text
-- import Text.Megaparsec

-- import DataTypes
-- import Parser
import Printer
import Solver
import Utils

test :: String -> Doc ann
test = prettyCalculation.calculate.parExpr

main :: IO ()
main = do
  -- putStrLn "Derivative Laws:"
--   putStrLn (show sampleLaws)
--  putStrLn "Derive" ++ expr 
  -- putStrLn "= " ++ sampleLawName
  -- putStrLn expr
  putStr "> "
  hFlush stdout
  -- s <- getLine
  -- when (s /= "") $ do
  --   putStrLn (show (parse pDeriv "" s))
  --   main
  return ()
{-
  (case (parse pDeriv "" s) of
      (Right e) -> let pprint = pretty (calculate e)
                   in do putDoc pprint
                         putStrLn "Your solution is saved in your-answer.md"
                         withFile "your-answer.md" WriteMode (\h -> do 
                                                                 hPutStrLn  h "# Your homework problem is solved!\n\n```"
                                                                 hPutDoc h pprint
                                                                 hPutStrLn h "```")
      (Left e) -> putStrLn (show e))
--  putStrLn ("= " ++  "{ " ++ (show sampleLawName) ++ " }")
  -- putStrLn "  x"
    
-}
