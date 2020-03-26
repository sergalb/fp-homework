{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tree
  ( Tree(..)
  , isEmpty
  , size
  , findElement
  , insertElement
  , fromList
  , removeElement
  ) where

import Numeric.Natural (Natural)

data Tree a
  = Leaf
  | Node Integer a (Tree a) (Tree a)
  deriving (Show)

instance Eq a => Eq (Tree a) where
  (==) Leaf Leaf                                                               = True
  (==) (Node lCount lElement lLeft lRight) (Node rCount rElement rLeft rRight) =
    lCount == rCount && lElement == rElement && lLeft == rLeft && lRight == rRight
  (==) _ _                                                                     = False

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Natural
size Leaf                      = 0
size (Node count _ left right) = fromInteger count + size left + size right

findElement :: Ord a => Tree a -> a -> Either String (Tree a)
findElement node@(Node _ element left right) x
  | element == x = Right node
  | element > x = findElement left x
  | otherwise = findElement right x
findElement _ _ = Left "tree doesn't contains requested element"

insertElement :: Ord a => Tree a -> a -> Tree a
insertElement leaf@Leaf x = Node 1 x leaf Leaf
insertElement (Node count element left right) x
  | element == x = Node (count + 1) element left right
  | element > x = Node count element (insertElement left x) right
  | otherwise = Node count element left (insertElement right x)

fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList (x:xs) = insertElement (fromList xs) x

mergeTree :: Ord a => Tree a -> Tree a -> Tree a
mergeTree Leaf x = x
mergeTree x Leaf = x
mergeTree left@(Node lCount lElement lLeft lRight) right@(Node rCount rElement rLeft rRight)
  | lElement == rElement = Node (lCount + rCount) lElement (mergeTree lLeft rLeft) (mergeTree lRight rRight)
  | lElement > rElement = Node rCount rElement (mergeTree left rLeft) rRight
  | otherwise = Node lCount lElement lLeft (mergeTree lRight right)

removeElement :: Ord a => Tree a -> a -> Either String (Tree a)
removeElement (Node count element left right) x
  | element == x =
    if count == 1
      then case (left, right) of
             (Leaf, Leaf)            -> Right Leaf
             (leftChild, Leaf)       -> Right leftChild
             (Leaf, rightChild)      -> Right rightChild
             (leftChild, rightChild) -> Right (mergeTree leftChild rightChild)
      else Right (Node (count - 1) element left right)
  | element > x = let removedLeft = removeElement left x
                    in case removedLeft of
                      Left _ -> removedLeft
                      Right subtree -> Right (Node count element subtree right)
  | otherwise = let removedRight = removeElement right x
                    in case removedRight of
                      Left _ -> removedRight
                      Right subtree -> Right (Node count element left subtree)
removeElement _ _ = Left "tree doesn't contains requested element"



instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ start Leaf = start
  foldr fun start (Node count element left right) =
    let rightRes = foldr fun start right
        curRes = foldrMyList fun rightRes count element
     in foldr fun curRes left
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap fun = foldr (mappend . fun) mempty

foldrMyList :: (a -> b -> b) -> b -> Integer -> a -> b
foldrMyList _ start 0 _           = start
foldrMyList fun start count value = fun value (foldrMyList fun start (count - 1) value)
