module Task3
  ( composition
  , identity
  , contraction
  , permutation
  ) where

-- | S combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | B combinator, similar to '.' operator
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | I combinator, similar to 'id' function
identity :: a -> a
identity = s const const

-- | W combinator
contraction :: (a -> a -> b) -> a -> b
contraction = s s (const (s const const))

-- | C combinator
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const s) const (s (const s) const) s) (const const)