{-# LANGUAGE LambdaCase #-}

{-$ LANGUAGE ScopedTypeVariables #-}
module Block6.Parsers.Task2
  ( ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import Block6.Parsers.Task1 (Parser (..))

-- | Parser witch accept any input
-- and return ((), input) on success
ok :: Parser s ()
ok = Parser $ \listS -> Just ((), listS)

-- | Parser witch accept only empty input
-- return ((), []) on success
eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

-- | Take predicate and create a Parser witch accept
-- input, started on element, witch accept by predicate
-- and return (head input, tail input) on success
satisfy :: (s -> Bool) -> Parser s s
satisfy check =
  Parser $ \case
    (x:xs)
      | (check x) -> Just (x, xs)
    _ -> Nothing

-- | Take element and create a Parser witch accept
-- input, started on element
-- and return (head input, tail input) on success
element :: Eq s => s -> Parser s s
element s = satisfy (== s)

-- | Take list of elements and create a Parser witch accept
-- input, started on given list
-- and return (list, tail of input after list) on success
stream :: Eq s => [s] -> Parser s [s]
stream = foldr (\x -> (<*>) (toList <$> element x)) (pure [])
  where
    toList x list = x : list