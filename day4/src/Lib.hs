module Lib
    ( checkPassphrases
    , checkPassphrases2
    ) where

import Data.List

{-

Day 4:
Given a list of passphrases, determine if the passphrase is valid.
A passphrase is valid if none of the words in the phrase are the same

-}

checkLine :: [String] -> Bool
checkLine [] = True
checkLine (w:ws) = w `notElem` ws && checkLine ws

checkPassphrases :: [[String]] -> Int
checkPassphrases = length . filter id . map checkLine

{-

Part 2:
Same, but passphrases cannot be anagrams of each other either

-}

checkPassphrases2 :: [[String]] -> Int
checkPassphrases2 = length . filter id . map (checkLine . map sort)

