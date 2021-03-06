module Lib
    ( solveCaptcha
    , solveCaptcha2
    ) where

{-

Day 1:
Solve a "captcha" which consists of a list of numbers.
To solve the captcha, produce the sum of all numbers in the list which match
the next number in the list. The head of the list is next to the tail in this
case.

-}

solveCaptcha :: [Int] -> Int
solveCaptcha list =
  let shifted = (tail list) ++ [head list]
      final = zipWith (\x y -> if x == y then x else 0) list shifted
  in sum final

{-

Part 2:
Solve the captcha, but instead of the next element, the element to check is
halfway around the list. The list will always have an even number of elements

-}

solveCaptcha2 :: [Int] -> Int
solveCaptcha2 list =
  let shifted = rotate (length list `div` 2) list
      final = zipWith (\x y -> if x == y then x else 0) list shifted
  in sum final
  where rotate n xs = zipWith const (drop n (cycle xs)) xs
