module Lib
    ( findDistance
    ) where

import Control.Monad.State.Lazy

{-

Day 3:
Find the distance from the center of a number on a spiral shaped like this:
5<-4<-3
v     ^
6  1->2
v
7->8->9
Given the number n. (i.e. d=2 for n=3)

-}

findCoords :: Int -> (Int, Int)
findCoords 1 = (0,0)
findCoords n =
  let k = head [x | x <- [1,3..] , x^2 >= n]
      m = n - (k-2)^2
      q = (m `div` (k-1)) `mod` 4
      d = m `mod` (k-1)
      j = (k-1) `div` 2
      r = abs (d-j) + j
  in case q of
       0 -> (j, -j+d)
       1 -> (j-d, j)
       2 -> (-j, j-d)
       3 -> (-j+d, -j)

findCoordDistance :: (Int, Int) -> Int
findCoordDistance c = (abs $ fst c) + (abs $ snd c)

findDistance :: Int -> Int
findDistance = findCoordDistance . findCoords

{-

Part 2:
Using the sum of all adjacent squares in the grid recursively, starting from the
middle and spiraling out, find the first value written that is larger than the
input.

-}

data Zipper a = Zipper [a] a Int [a]
instance Functor Zipper where
  fmap f (Zipper l c n r) = Zipper (map f l) (f c) n (map f r)
instance Show a => Show (Zipper a) where
  show (Zipper ls x n rs) =
    show (reverse (take 5 ls)) ++ " " ++ show (x,n) ++ " " ++ show (take 5 rs)

back, forth :: Zipper a -> Zipper a
back (Zipper (l:ls) x n rs) = Zipper ls l (n-1) (x:rs)
forth (Zipper ls x n (r:rs)) = Zipper (x:ls) r (n+1) rs

newtype Grid a = Grid (Zipper (Zipper a))
instance Show a => Show (Grid a) where
  show (Grid (Zipper ls x n rs)) =
    unlines $ zipWith (\a b -> a ++ " " ++ b)
              (map show [n+5,n+4..n-5])
              (map show (reverse (take 5 rs) ++ [x] ++ (take 5 ls)))

right, left, up, down :: Grid a -> Grid a
right (Grid g) = Grid (fmap forth g)
left (Grid g) = Grid (fmap back g)
up (Grid g) = Grid (forth g)
down (Grid g) = Grid (back g)

moveTo :: (Int, Int) -> Grid a -> Grid a
moveTo (x,y) g@(Grid (Zipper _ (Zipper _ _ m _) n _))
  | n < y = moveTo (x,y) (up g)
  | n > y = moveTo (x,y) (down g)
  | m < x = moveTo (x,y) (right g)
  | m > x = moveTo (x,y) (left g)
  | otherwise = g

recenter :: Grid a -> Grid a
recenter = moveTo (0,0)

setGrid :: a -> Grid a -> Grid a
setGrid y (Grid (Zipper ls x n rs)) = (Grid (Zipper ls (set' x) n rs))
  where set' (Zipper ls' z n rs') = Zipper ls' y n rs'

getGrid :: Grid a -> a
getGrid (Grid (Zipper _ (Zipper _ x _ _) _ _)) = x

(&) = flip ($)

zeroGrid :: Grid Int
zeroGrid =
  let zeroes = repeat 0
      zeroRow = Zipper zeroes 0 0 zeroes
      zeroRows = repeat zeroRow
  in Grid (Zipper zeroRows zeroRow 0 zeroRows)

initialGrid :: Grid Int
initialGrid = zeroGrid & setGrid 1

sumNeighbors :: Grid Int -> Int
sumNeighbors g =
  let ul = g & up & left & getGrid
      uc = g & up & getGrid
      ur = g & up & right & getGrid
      cl = g & left & getGrid
      cr = g & right & getGrid
      ll = g & down & left & getGrid
      lc = g & down & getGrid
      lr = g & down & right & getGrid
  in ul + uc + ur + cl + cr + ll + lc + lr

stressTest :: Int -> State ((Grid Int), Int) Int
stressTest target = do
  (oldGrid, oldIndex) <- get

  let nextIndex = oldIndex+1
  let coords = findCoords nextIndex
  let g = moveTo coords oldGrid
  let val = sumNeighbors g
  return val
  let nextGrid = setGrid val g & recenter

  put (nextGrid, nextIndex)
  if val > target
  then return val
  else stressTest target

