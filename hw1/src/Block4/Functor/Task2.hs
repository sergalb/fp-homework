{-# LANGUAGE InstanceSigs #-}
module Block4.Functor.Task2 where

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f tree =
    case tree of
      Leaf x                   -> Leaf (f x)
      Branch left right -> Branch (fmap f left) (fmap f right)
