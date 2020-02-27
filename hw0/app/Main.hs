{-# LANGUAGE TypeOperators #-}

module Main where

import Task5

main :: IO ()
main = putStrLn $ show $ churchToInt $ churchPlus (\f x -> f $ f $ f $ f x :: Integer) (\f x -> f $ f $ f x :: Integer)

