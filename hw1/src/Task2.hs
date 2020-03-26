module Task2
  ( Nat(..)
  , hw1Sum
  , hw1Mul
  , hw1Sub
  , natToInteger
  , integerToNat
  ) where

data Nat
  = Z
  | S Nat

instance Num Nat where
  (+) = hw1Sum
  (*) = hw1Mul
  (-) = hw1Sub
  fromInteger = integerToNat
  negate _ = Z
  abs x = x
  signum Z = Z
  signum _ = S Z

instance Show Nat where
  show = show . natToInteger

instance Eq Nat where
  (==) Z Z         = True
  (==) (S a) (S b) = a == b
  (==) _ _         = False

instance Ord Nat where
  (<=) Z _         = True
  (<=) (S _) Z     = False
  (<=) (S a) (S b) = a <= b

hw1Sum :: Nat -> Nat -> Nat
hw1Sum Z b         = b
hw1Sum a Z         = a
hw1Sum (S a) (S b) = S $ S $ hw1Sum a b

hw1Mul :: Nat -> Nat -> Nat
hw1Mul Z _     = Z
hw1Mul _ Z     = Z
hw1Mul a (S Z) = a
hw1Mul a (S b) = (hw1Mul a b) `hw1Sum` a

hw1Sub :: Nat -> Nat -> Nat
hw1Sub Z (S _)     = Z
hw1Sub a Z         = a
hw1Sub (S a) (S b) = hw1Sub a b

natToInteger :: Nat -> Integer
natToInteger Z     = 0
natToInteger (S x) = natToInteger x + 1

integerToNat :: Integer -> Nat
integerToNat 0 = Z
integerToNat x =
  if x > 0
    then S $ integerToNat (x - 1)
    else error (show x ++ " not natural number")
