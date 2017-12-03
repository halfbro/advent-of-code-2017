module Main where

import Lib
import System.Environment (getArgs)

readData :: String -> [[Int]]
readData = fmap (map read . words) . lines

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let input = readData content
  let solution = solveChecksum2 input
  print solution
  return ()
