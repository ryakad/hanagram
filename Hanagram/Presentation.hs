-- Haskell Anagram Library
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
--
-- Functions for presenting the results to the user
--
-- Author: Ryan Kadwell <ryan@riaka.ca>

module Hanagram.Presentation (
      showResults
    , sayResults
    ) where

import System.Cmd
import System.Exit

-- | Display the matches to the user one per line
showResults :: [String] -> IO ()
showResults [] = return ()
showResults (x:xs) = do
    putStrLn x
    showResults xs

-- | Uses OSx text to speech to say the matches to the user
sayResults :: [[Char]] -> IO ExitCode
sayResults [] = return ExitSuccess
sayResults (x:xs) = do
    system $ "say " ++ x ++ " "
    sayResults xs
