{-# LANGUAGE ScopedTypeVariables #-}

module Task4
  ( iterateElement
  , fibonacci
  , factorial
  ) where

import Data.Function (fix)

-- | Create a infinity list with repeated given element.
-- Return created list.
iterateElement :: a -> [a]
iterateElement element = fix (\rec [xs :: a] -> element : rec [xs]) []

-- | Evaluate x'th fibonacci number.
-- x must be non-negative.
fibonacci :: Integer -> Integer
fibonacci x =
  if x < 0
    then -1
    else fix
           (\rec n ->
              case n of
                0 -> 0
                1 -> 1
                _ -> rec (n - 1) + rec (n - 2))
           x

-- | Evaluate x!. x must be non-negative.
factorial :: Integer -> Integer
factorial x =
  if x < 0
    then -1
    else fix
           (\rec n ->
              if n <= 1
                then 1
                else n * rec (n - 1))
           x

-- | Map all element of given list with given function.
-- Return list with mapped values.
mapFix :: (a -> b) -> [a] -> [b]
mapFix f start =
  fix
    (\rec (arg:args, res) ->
       case args of
         [] -> f arg : res
         _  -> rec (args, f arg : res))
    (start, [])
