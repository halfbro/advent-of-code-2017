module Main where

import Lib
import System.Environment (getArgs)
import Data.Char (digitToInt)

main :: IO ()
main = do
  args <- getArgs
  let inputString = case args of
        [] -> error "No input given, input should be a string of numerals"
        l:ls -> l
  let input = map digitToInt inputString
  let solution = solveCaptcha2 input
  print solution
