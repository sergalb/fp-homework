module Block3.Monoids.Task2 where

data NonEmpty a =
  a :| [a]
  deriving (Show, Eq)

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show, Eq)

toList :: NonEmpty a -> [a]
toList (head :| tail) = head : tail

fromList :: [a] -> NonEmpty a
fromList (x:xs) = x :| xs
fromList _      = error "require non emty list, but get empty"

instance Semigroup (NonEmpty a) where
  (<>) left right = fromList $ toList left ++ toList right

instance Semigroup (ThisOrThat a b) where
  (<>) (This x) (That y) = Both x y 
  (<>) (That x) (This y) = Both y x 
  (<>) (Both x _) (That y) = Both x y 
  (<>) (Both _ x) (This y) = Both y x 
  (<>) _ right = right