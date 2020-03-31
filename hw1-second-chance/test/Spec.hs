module Main where

import Test.Tasty (defaultMain, testGroup)

import Block6.Parsers.Tests(tests) 
 

main :: IO ()
main = tests >>= \unitTests ->
       let allTests = testGroup "Parser" [unitTests]
       in defaultMain allTests