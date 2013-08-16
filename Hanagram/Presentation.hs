-- Haskell Anagram Library
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
--
-- For the full copyright and license information, please view the LICENSE
-- file that was distributed with this source code.
--
-- Functions for presenting the results to the user
--
-- Author: Ryan Kadwell <ryan@riaka.ca>

module Hanagram.Presentation (
      showResults
    , sayResults
    ) where

import System.Cmd
import System.Exit (ExitCode(..))

-- | Display the matches to the user one per line
showResults :: [String] -> IO ()
showResults [] = return ()
showResults (x:xs) = do
    putStrLn x
    showResults xs

-- | Uses OSx text to speech to say the matches to the user
sayResults :: [[Char]] -> IO ()
sayResults [] = return ()
sayResults (x:xs) = do
    system $ "say " ++ x ++ " "
    sayResults xs
