-- Haskell Anagram Library
-- Copyright (c) Ryan Kadwell <ryan@riaka.ca>
--
-- Unit tests for Hanagram module
--
-- Author: Ryan Kadwell <ryan@riaka.ca>


import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Hanagram

-- list of words to test against
wordList = ["time", "person", "year", "way", "day", "thing", "man", "world",
    "life", "hand", "part", "child", "eye", "woman", "place", "work", "week",
    "case", "point", "government", "company", "number", "group", "problem",
    "fact", "workweek"]

main :: IO ()
main = hspec $ do
    describe "Hanagram.hs" $ do
        it "can find matches" $ do
            getMatches "test" [] `shouldBe` []
            getMatches "personart" wordList `shouldBe` ["person", "part"]
            getMatches "work" wordList `shouldBe` ["work"]
            getMatches "" wordList `shouldBe` []
