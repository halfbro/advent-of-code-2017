module Lib
    ( solveChecksum
    , solveChecksum2
    ) where

{-

Day 2:
Perform a checksum of a spreadsheet where the checksum is the sum of the largest
difference in values for each row

-}

solveChecksum :: [[Int]] -> Int
solveChecksum input =
  let diff l = (maximum l) - (minimum l)
  in sum $ map diff input

{-

Part 2:
Find the value of the only two evenly divisible values in each row, and
use the result of the division as the value for the checksum. Sum the rows.

-}

findDivisibles :: [Int] -> Int
findDivisibles l =
  let combinations = [x | x <- mapM (const l) [1..2], x!!0 > x!!1 ]
      valid = [x | x <- combinations, rem (x!!0) (x!!1) == 0 ]
      nums = ((head valid)!!0, (head valid)!!1)
  in div (fst nums) (snd nums)

solveChecksum2 :: [[Int]] -> Int
solveChecksum2 input =
  let lineval = findDivisibles
  in sum $ map lineval input
