module Block3.Monoids.Task2Tests(block3Task2Tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty as NE

import Block3.Monoids.Task2 as T2

block3Task2Tests :: TestTree
block3Task2Tests =
  testGroup
    "block3Task2"
    [semigroupNonEmptyTests]

semigroupNonEmptyTests :: TestTree
semigroupNonEmptyTests =
  testGroup
    "semigroupNonEmpty"
    [ testCase "simple case" $ NE.toList (NE.fromList [1..5] <> NE.fromList [2,4..10]) @=? T2.toList (T2.fromList [1..5] <> T2.fromList [2, 4..10])

    ]



