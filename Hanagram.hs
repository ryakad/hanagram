-- Haskell Anagram Library
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
--
-- Main hanagram module.
--
-- Author: Ryan Kadwell <ryan@riaka.ca>

-- | Provides support for processing anagrams in haskell
module Hanagram (
      getMatches
    , getMatchesDups
    ) where

import Data.List
import Data.Tuple

-- | Takes a string of letters and a list of possible words and returns the
-- words that can be formed using the letters without using any letter more
-- than once
getMatches :: String -> [String] -> [String]
getMatches letters words =
    [ word | word <- words, canMake word letters False ]

-- | Takes a string of letters and a list of possible words and returns the
-- words that can be formed using the letters
getMatchesDups :: String -> [String] -> [String]
getMatchesDups letters words =
    [ word | word <- words, canMake word letters True ]

-- see if all letters in the word are in the array of letters
canMake :: String -> String -> Bool -> Bool
canMake [] _ _ = True
canMake (x:xs) letters allowDups =
    if x `elem` letters
    then canMake xs (if allowDups then letters else removeOne x letters) allowDups
    else False

-- remove one character from the string to give us matches that do not allow
-- duplicate letters in the matching
removeOne :: Char -> String -> String
removeOne _ [] = []
removeOne x letters = first ++ rest
    where first = fst $ span (/= x) letters
          rest = tail $ snd $ span (/= x) letters
