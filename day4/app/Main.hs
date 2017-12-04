module Main where

import Lib
import System.Environment

readData :: String -> [[String]]
readData = map words . lines

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let input = readData content
  let solution = checkPassphrases2 input
  print solution
