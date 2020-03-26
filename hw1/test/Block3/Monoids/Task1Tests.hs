module Block3.Monoids.Task1Tests (block3Task1Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Block3.Monoids.Task1

block3Task1Tests :: TestTree
block3Task1Tests =
  testGroup
    "block3Task1"
    [maybeConcatTests]

maybeConcatTests :: TestTree
maybeConcatTests =
  testGroup
    "maybeConcat"
    [ testCase "simple case" $ ([1,2,3,4,5] :: [Int]) @=? (maybeConcat [Just [1,2,3], Nothing, Just [4,5]] :: [Int])
    , testCase "empty result" $ ([] :: [Int]) @=? (maybeConcat [Nothing, Nothing, Nothing] :: [Int]) 
    , testCase "repeated" $ ([1,2,3,4,5,1,2,3,4,5] :: [Int]) @=? (maybeConcat [Just [1..5], Just [1..5]] :: [Int])

    ]

