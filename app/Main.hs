module Main where


-- import Control.Monad
import System.IO
import Text.Megaparsec

import Parser
import Solver

main :: IO ()
main = do
  putStrLn "Derivative Laws:"
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
  putStrLn (show (parse pDeriv "" s))
--  putStrLn ("= " ++  "{ " ++ (show sampleLawName) ++ " }")
  putStrLn "  x"
    
