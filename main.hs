-- Haskell Anagram Library
--
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
-- Author: Ryan Kadwell <ryan@riaka.ca>

module Main where

import System.Environment (getArgs)
import Data.List
import Data.Tuple
import System.Cmd
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile "/usr/share/dict/words"
    let words = lines contents
    let findLength = read (args !! 0) :: Int
    let matches = [ word | word <- getMatches (args !! 1) words, length word == findLength]
    showMatches matches
    sayMatches matches
    putStrLn ""

showMatches :: [String] -> IO ()
showMatches [] = do
    putStrLn ""
showMatches (x:xs) = do
    putStrLn x
    showMatches xs

sayMatches :: [[Char]] -> IO ExitCode
sayMatches [] = system $ "echo ''"
sayMatches (x:xs) = do
    system $ "say " ++ x ++ " "
    sayMatches xs

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
