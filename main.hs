-- Haskell Anagram Library
--
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
-- Author: Ryan Kadwell <ryan@riaka.ca>

module Main (main) where

import System.Environment (getArgs)
import Hanagram (getMatches, getMatchesNoDups)
import Hanagram.Presentation (showResults, sayResults)
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

data Options = Options {
      optSpeech :: Bool
    , optNoDups :: Bool
    , optLength :: Maybe String
    , optLetters :: String
    } deriving (Show)

defaultOptions = Options {
      optSpeech = False
    , optNoDups = False
    , optLength = Nothing
    , optLetters = []
    }

options :: [OptDescr (Options -> Options)]
options = [
      Option ['s'] ["say"]
        (NoArg (\ opts -> opts { optSpeech = True }))
        "enable text to speech processing"
    , Option [] ["no-dups"]
        (NoArg (\ opts -> opts { optNoDups = True }))
        "when finding words that match do not allow letters to be used more than once"
    , Option [] ["length"]
        (OptArg ((\ f opts -> opts { optLength = Just f }) . fromMaybe "0" ) "INTEGER")
        "limit results to words of a specific length"
    , Option ['l'] ["letters"]
        (ReqArg (\ l opts -> opts { optLetters = l }) "STRING")
        "the available letters"
    ]

-- | parse the options from passed array into the opts object
parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
    case getOpt Permute options argv of
        (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: hanagram [options...] [dictionary files...]"

-- | Read an array of files and return all the words that are contained. This
-- allows us to pass dict files where it contains one word per line or just
-- regular space delimited files
readFiles :: [FilePath] -> IO [String]
readFiles [] = return []
readFiles (x:xs) = do
    contents <- readFile x
    let foundWords = words contents
    rest <- readFiles xs
    return $ foundWords ++ rest

fixLength :: Maybe String -> Int
fixLength Nothing  = 0
fixLength (Just x) = (read x :: Int)

main :: IO ()
main = do
    args <- getArgs
    (opts, files) <- parseOpts args
    words <- readFiles files

    let getMatchesFunction = if optNoDups opts
        then getMatchesNoDups
        else getMatches

    let searchLength = fixLength (optLength opts)
    let matches = if searchLength == 0
        then [ word | word <- getMatchesFunction (optLetters opts) words]
        else [ word | word <- getMatchesFunction (optLetters opts) words, length word == searchLength]

    showResults matches
