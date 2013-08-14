-- Haskell Anagram Library
--
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
-- Author: Ryan Kadwell <ryan@riaka.ca>

module Main where

import System.Environment (getArgs)
import Hanagram (showMatches, sayMatches, getMatches)

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
