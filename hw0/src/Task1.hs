{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

-- | inhabit A | (B & C) -> (A | B) & (A | C) type
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left (x :: a))            = (Left x :: Either a b, Left x :: Either a c)
distributivity (Right ((x, y) :: (b, c))) = (Right x :: Either a b, Right y :: Either a c)

-- | inhabit (A & (B & C) -> (A & B) & C type
associator :: (a, (b, c)) -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

-- | inhabit A | (B | C) <-> (A | B) |  C type
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc =
  ( \case
      (Left a) -> Left (Left a)
      (Right (Left b)) -> Left (Right b)
      (Right (Right c)) -> Right c
  , \case
      (Left (Left a)) -> Left a
      (Left (Right b)) -> Right (Left b)
      (Right c) -> Right (Right c))