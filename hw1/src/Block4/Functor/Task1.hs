module Block4.Functor.Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum string =
  let listWords = words string
   in fmap sum (traverse safeTakeIntSum listWords)
  where
    safeTakeIntSum word = readMaybe word :: Maybe Int
