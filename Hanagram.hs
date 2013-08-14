-- Haskell Anagram Library
--
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
-- Author: Ryan Kadwell <ryan@riaka.ca>

-- | Provides support for processing anagrams in haskell
module Hanagram (
      showMatches
    , sayMatches
    , getMatches
    ) where

import Data.List
import Data.Tuple
import System.Cmd
import System.Exit

-- | Display the matches to the user one per line
showMatches :: [String] -> IO ()
showMatches [] = do
    putStrLn ""
showMatches (x:xs) = do
    putStrLn x
    showMatches xs

-- | Uses OSx text to speech to say the matches to the user
sayMatches :: [[Char]] -> IO ExitCode
sayMatches [] = system $ "echo ''"
sayMatches (x:xs) = do
    system $ "say " ++ x ++ " "
    sayMatches xs

-- | Takes a string of letters and a list of all possible words in the
-- dictionary and returns the ones that can eb formed from the list
getMatches :: String -> [String] -> [String]
getMatches letters words =
    [ word | word <- words, canMake word letters ]

-- see if all letters in the word are in the array of letters
canMake :: String -> String -> Bool
canMake [] _ = True
canMake (x:xs) letters = if x `elem` letters
                         then canMake xs $ removeOne x letters
                         else False

-- remove one character from the string to give us matches that do not allow
-- duplicate letters in the matching
removeOne :: Char -> String -> String
removeOne _ [] = []
removeOne x letters = first ++ rest
    where first = fst $ span (/= x) letters
          rest = tail $ snd $ span (/= x) letters
