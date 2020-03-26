module Block2.Task2Tests
  ( block2Task2Tests
  ) where

import Data.List.NonEmpty (toList)
import Test.Tasty
import Test.Tasty.HUnit

import Block2.Task2

block2Task2Tests :: TestTree
block2Task2Tests =
  testGroup
    "block2Task2"
    [splitOnTests]

splitOnTests :: TestTree
splitOnTests =
  testGroup
    "splitOn"
    [ testCase "empty result" $ [""] @=? toList (splitOn '/' "")
    , testCase "empty result with separators inpt" $ [""] @=? toList ( splitOn '/' "/////")
    , testCase "non empty result" $ ["path", "to", "file"] @=? toList (splitOn '/' "path/to/file")
    ]
