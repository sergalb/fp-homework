{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block4.Functor.Task2
  ( Tree(..)
  ) where

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)            = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf fun <*> tree = fun <$> tree
  Branch leftFun rightFun <*> tree = Branch (leftFun <*> tree) (rightFun <*> tree)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap fun (Leaf x)            = fun x
  foldMap fun (Branch left right) = foldMap fun left `mappend` foldMap fun right

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse fun (Leaf x)            = Leaf <$> fun x
  traverse fun (Branch left right) = Branch <$> traverse fun left <*> traverse fun right
