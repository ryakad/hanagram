-- Haskell Anagram Library
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
--
-- For the full copyright and license information, please view the LICENSE
-- file that was distributed with this source code.
--
-- Author: Ryan Kadwell <ryan@riaka.ca>

module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (sortBy, nub)
import Data.Maybe (fromMaybe)
import Hanagram (getMatches, getMatchesDups)
import Hanagram.Presentation (showResults, sayResults, showAndSayResults)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

-- | Return the hanagram version string
versionString :: String
versionString = "\nHanagram v1.0\nCopyright (c) Ryan Kadwell 2013 <ryan@riaka.ca>\n"

-- | Returns the hanagram usage information
usageString :: [OptDescr (Options -> Options)] -> String
usageString options = usageInfo header options
    where header = "\nUsage: hanagram [options...] [dictionary files...]"

data Options = Options {
      optVersion :: Bool
    , optHelp :: Bool
    , optOrdered :: Bool
    , optSpeech :: Bool
    , optDups :: Bool
    , optLength :: Maybe String
    , optLetters :: String
    } deriving (Show)

defaultOptions = Options {
      optVersion = False
    , optHelp = False
    , optOrdered = False
    , optSpeech = False
    , optDups = False
    , optLength = Nothing
    , optLetters = []
    }

options :: [OptDescr (Options -> Options)]
options = [
      Option "v" ["version"]
        (NoArg (\ opts -> opts { optVersion = True }))
        "Display version"
    , Option "h?" ["help"]
        (NoArg (\ opts -> opts { optHelp = True }))
        "Display help message"
    , Option "o" ["ordered"]
        (NoArg (\ opts -> opts { optOrdered = True }))
        "Display the output sorted based on word length"
    , Option "s" ["say"]
        (NoArg (\ opts -> opts { optSpeech = True }))
        "Enable text to speech processing"
    , Option [] ["dups"]
        (NoArg (\ opts -> opts { optDups = True }))
        "When finding words that match allow letters to be used more than once"
    , Option "n" ["length"]
        (OptArg ((\ f opts -> opts { optLength = Just f }) . fromMaybe "0" ) "INTEGER")
        "Limit results to words of a specific length"
    , Option "l" ["letters"]
        (ReqArg (\ l opts -> opts { optLetters = l }) "STRING")
        "The available letters"
    ]

-- | parse the options from passed array into the opts object
parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
    case getOpt Permute options argv of
        (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageString options))

-- | Read an array of files and return all the lines that are contained.
-- This allows us to pass dict files where it contains one word per line or
-- just regular space delimited files
parseFiles :: [FilePath] -> IO [String]
parseFiles [] = return []
parseFiles (x:xs) = do
    contents <- readFile x
    let returnLines = lines contents
    rest <- parseFiles xs
    return $ returnLines ++ rest

-- converts a maybe string into the length limit or 0 if we dont care about
-- the length
fixLength :: Maybe String -> Int
fixLength Nothing  = 0
fixLength (Just x) = (read x :: Int)

main :: IO (ExitCode)
main = do
    args <- getArgs
    (opts, files) <- parseOpts args

    when (optHelp opts) $ do
        putStrLn $ versionString ++ (usageString options)
        exitWith ExitSuccess

    when (optVersion opts) $ do
        putStrLn versionString
        exitWith ExitSuccess

    words <- parseFiles files

    when (words == []) $ do
        putStrLn "hanagram: error: No words to check against. \nEnsure you have provided a path to a usable dictionary file"
        exitWith $ ExitFailure 1

    let getMatchesFunction = if optDups opts then getMatchesDups else getMatches

    -- need case insesitive searching
    let filteredWords = [ map toLower word | word <- words ]

    let searchLength = fixLength (optLength opts)
    let matches = if searchLength == 0
        then [ word | word <- getMatchesFunction (optLetters opts) filteredWords]
        else [ word | word <- getMatchesFunction (optLetters opts) filteredWords, length word == searchLength]

    let sortedMatches = if optOrdered opts
        then sortBy (compare `on` length) matches
        else matches

    -- show results omiting any duplicates. We filter duplicates at this
    -- point because its not a cheap process...
    let filteredResults = nub sortedMatches
    if optSpeech opts
        then showAndSayResults filteredResults
        else showResults filteredResults

    return ExitSuccess
