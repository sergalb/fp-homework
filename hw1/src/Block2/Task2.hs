{-# LANGUAGE ScopedTypeVariables #-}

module Block2.Task2
  ( splitOn
  ) where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldl' (fun separator) (([] :: [a]) :| [])

fun :: Eq a => a -> NonEmpty [a] -> a -> NonEmpty [a]
fun separator sublist element =
  let list = toList sublist
   in if separator == element
        then if null $ last list
               then sublist
               else fromList (list ++ [[]])
        else let lastElem = last list
              in fromList $ init list ++ [lastElem ++ [element]]
