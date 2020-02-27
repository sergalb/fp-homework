{-# LANGUAGE ScopedTypeVariables #-}

module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | inhabit A -> !!A type
doubleNeg :: a -> Neg (Neg a)
doubleNeg x (f :: Neg a) = f x

-- | inhabit  !!(A | !A) type
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = (f . Right) (f . Left)

-- | Not inhabited
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | Not inhabited
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | inhabit !!!A -> !A
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim x y = x (doubleNeg y)