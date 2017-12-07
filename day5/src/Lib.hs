{-# LANGUAGE DeriveFunctor #-}
module Lib
    ( findSteps
    , findSteps2
    ) where

{-

Day 5:
Given a list of integers, find how long it will take to exit the list, where the
integer in the position you are at moves you forward (back) for positive
(negative) numbers, then increments the previous number by 1

-}

data Zipper a = Zipper [a] a [a] deriving (Functor)
instance (Show a) => Show (Zipper a) where
  show (Zipper ls x rs) =
    show (reverse (take 10 ls)) ++ " " ++ show x ++ " " ++ show (take 10 rs)

back, forth :: Zipper a -> Maybe (Zipper a)
back (Zipper [] _ _ ) = Nothing
back (Zipper (l:ls) x rs) = Just $ Zipper ls l (x:rs)
forth (Zipper _ _ []) = Nothing
forth (Zipper ls x (r:rs)) = Just $ Zipper (x:ls) r rs

get :: Zipper a -> a
get (Zipper _ x _) = x

set :: a -> Zipper a -> Zipper a
set n (Zipper ls _ rs) = Zipper ls n rs

jump :: Int -> Zipper a -> Maybe (Zipper a)
jump n z
  | n == 0 = Just z
  | n < 0  = do
      b <- back z
      jump (n+1) b
  | n > 0  = do
      f <- forth z
      jump (n-1) f

followProgram :: (Int, Zipper Int) -> Int
followProgram (i, z) =
  let n = get z
      newz = set (n+1) z
  in case jump n newz of
      Nothing -> i+1
      Just jumpedz -> followProgram (i+1, jumpedz)

findSteps :: [Int] -> Int
findSteps (l:ls) =
  let z = Zipper [] l ls
  in followProgram (0, z)

{-

Part 2:
If the offset was 3  or more, decrease it by 1 instead of increase.
Otherwise increase it by 1

-}

followProgram2 :: (Int, Zipper Int) -> Int
followProgram2 (i, z) =
  case jump n newz of
      Nothing -> i+1
      Just jumpedz -> followProgram2 (i+1, jumpedz)
  where
    n = get z
    newz | n >= 3 = set (n-1) z
         | otherwise = set (n+1) z

findSteps2 :: [Int] -> Int
findSteps2 (l:ls) =
  let z = Zipper [] l ls
  in followProgram2 (0, z)
