{-# LANGUAGE ScopedTypeVariables #-}

module Task5
  ( zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

-- | Define 0 in church numeral
zero :: Nat a
zero _ x = x

-- | Function (+ 1) for church numeral
succChurch :: Nat a -> Nat a
succChurch n f x = n f (f x)

-- | Function (+) for church numeral
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus x y f n = x f (y f n)

-- | Function (*) for church numeral
churchMult :: Nat a -> Nat a -> Nat a
churchMult x y f = x (y f)

-- | Evaluate Integer representation of given church numeral
churchToInt :: Nat Integer -> Integer
churchToInt x = x (1 +) 0
