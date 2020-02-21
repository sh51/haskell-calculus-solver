module Main where


import Control.Monad
import System.IO
import Text.Megaparsec

import Parser
import Solver

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  s <- getLine
  when (s /= "") $ do
    putStrLn (show (parse pDeriv "" s))
    main


