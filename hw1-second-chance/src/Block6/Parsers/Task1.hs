{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block6.Parsers.Task1
  ( Parser(..)
  , empty
  , (<|>)
  ) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad ((>=>))

data Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

first :: (a -> b) -> ((a, [s]) -> (b, [s]))
first f (fst, snd :: [s]) = (f fst, snd)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f parser = Parser (fmap (first f) . runParser parser)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \listS -> pure (x, listS)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  fun <*> parser = Parser $ runParser fun >=> (\(f :: a -> b, endS :: [s]) -> first f <$> runParser parser endS)

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  left <|> right = Parser $ \s -> runParser left s <|> runParser right s

instance Monad (Parser s) where
  return = pure
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  val >>= fun =
    Parser $ \s ->
      case runParser val s of
        Nothing       -> Nothing
        Just (x, end) -> runParser (fun x) end
