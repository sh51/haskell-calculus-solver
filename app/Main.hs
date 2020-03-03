module Main where

import System.IO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Text.Megaparsec

-- import DataTypes
import Parser
import Printer ()
import Solver

main :: IO ()
main = do
  -- putStrLn "Derivative Laws:"
--   putStrLn (show sampleLaws)
--  putStrLn "Derive" ++ expr 
  -- putStrLn "= " ++ sampleLawName
  -- putStrLn expr
  putStr "> "
  hFlush stdout
  s <- getLine
  -- when (s /= "") $ do
  --   putStrLn (show (parse pDeriv "" s))
  --   main
  (case (parse pDeriv "" s) of
      (Right e) -> putDoc (pretty (calculate e))
      (Left e) -> putStrLn (show e))
--  putStrLn ("= " ++  "{ " ++ (show sampleLawName) ++ " }")
  -- putStrLn "  x"
    
