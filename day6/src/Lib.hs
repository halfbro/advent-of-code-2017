{-# LANGUAGE DeriveFunctor #-}

module Lib
    ( findRedistributionCycles
    , findRedistributionCycles2
    ) where

{-

Day 6:
Given 16 memory banks which can hold any number of blocks, run a reallocation
routine to find out after how long the routine repeats itself.

-}

data Zipper a = Zipper [a] a [a] deriving(Functor)
instance (Show a) => Show (Zipper a) where
  show (Zipper ls x rs) =
    show (reverse (take 10 ls)) ++ " " ++ show x ++ " " ++ show (take 10 rs)
instance (Eq a) => Eq (Zipper a) where
  (Zipper ls1 n1 rs1) == (Zipper ls2 n2 rs2) = (ls1 == ls2) && (n1 == n2) && (rs1 == rs2)

back, forth :: Zipper a -> Zipper a
back (Zipper [] x rs) = Zipper t n []
  where t = tail rev
        n = head rev
        rev = reverse (x:rs)
back (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
forth (Zipper ls x []) = Zipper [] n t
  where t = tail rev
        n = head rev
        rev = reverse (x:ls)
forth (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

get :: Zipper a -> a
get (Zipper _ x _) = x

set :: a -> Zipper a -> Zipper a
set n (Zipper ls _ rs) = Zipper ls n rs

-- End of Zipper generic stuff

incZip :: Zipper Int -> Zipper Int
incZip z = set (n+1) z
  where n = get z

distributeBlocks :: Int -> Zipper Int -> Zipper Int
distributeBlocks 0 = id
distributeBlocks n = distributeBlocks (n-1) . forth . incZip

findStart :: Zipper a -> Zipper a
findStart z@(Zipper [] _ _) = z
findStart z = findStart $ forth z

findMax :: Zipper Int -> Zipper Int -> Zipper Int
findMax zmax zcur
  | zmax == zcur = zmax
  | curmax > oldmax = findMax zcur $ forth zcur
  | otherwise = findMax zmax $ forth zcur
  where
    curmax = get zcur
    oldmax = get zmax

reallocate :: Zipper Int -> Zipper Int
reallocate z = distributeBlocks maxVal $ forth $ set 0 maxZip
  where
    start = findStart z
    maxZip = findMax start $ forth start
    maxVal = get maxZip

findCycles :: Int -> Zipper Int -> Zipper Int -> (Int, Zipper Int)
findCycles n z1 z2
  | sz1 == sz2 = (n, sz1)
  | otherwise = findCycles (n+1) (reallocate z1) (reallocate $ reallocate z2)
  where
    sz1 = findStart z1
    sz2 = findStart z2

findFirstCycle :: Int -> Zipper Int -> Zipper Int -> (Int, Zipper Int)
findFirstCycle n z1 z2
  | sz1 == sz2 = (n, sz1)
  | otherwise = findFirstCycle (n+1) (reallocate z1) (reallocate z2)
  where
    sz1 = findStart z1
    sz2 = findStart z2

findLambda :: Int -> Zipper Int -> Zipper Int -> Int
findLambda n z1 z2
  | sz1 == sz2 = n
  | otherwise = findLambda (n+1) z1 (reallocate z2)
  where
    sz1 = findStart z1
    sz2 = findStart z2

findRedistributionCycles :: [Int] -> Int
findRedistributionCycles xs =
  lam + mu
  where start = Zipper [] (head xs) (tail xs)
        (_,firstrepitition) = findCycles 0 (reallocate start) (reallocate $ reallocate start)
        (mu,next) = findFirstCycle 0 start firstrepitition
        lam = findLambda 1 next $ reallocate next

{-

Part 2:
Find the length of the repeating cycle

-}

findRedistributionCycles2 :: [Int] -> Int
findRedistributionCycles2 xs =
  lam
  where start = Zipper [] (head xs) (tail xs)
        (_,firstrepitition) = findCycles 0 (reallocate start) (reallocate $ reallocate start)
        (mu,next) = findFirstCycle 0 start firstrepitition
        lam = findLambda 1 next $ reallocate next
