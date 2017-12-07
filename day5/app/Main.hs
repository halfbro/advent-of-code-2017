module Main where

import Lib
import System.Environment

readData :: String -> [Int]
readData = map read . lines

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let input = readData content
  let solution = findSteps2 input
  print solution
