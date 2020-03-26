{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeTests
  ( treeTests
  ) where

import Data.Foldable (toList)
import Data.List
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit
import qualified Tree
import Data.Either (isLeft, fromRight)


treeTests :: TestTree
treeTests = testGroup "tree tests" 
  [ propertyFoldableTest
  , isEmptyTests
  , sizeTests
  , findElementTests
  , insertElementTests
  , fromListElementTests
  , removeElementTests
  ]

simpleTreeGenerator :: Integer -> Integer -> Tree.Tree Integer
simpleTreeGenerator count value = Tree.Node count value Tree.Leaf Tree.Leaf

treeWithTwoChild :: Tree.Tree Integer
treeWithTwoChild = Tree.Node 1 1 (simpleTreeGenerator 1 0) (simpleTreeGenerator 1 2)

simpleTreeWithRepeatedElement :: Tree.Tree Integer
simpleTreeWithRepeatedElement = simpleTreeGenerator 17 42

isEmptyTests :: TestTree
isEmptyTests =
  testGroup
    "isEmpty"
    [ testCase "one leaf - empty tree" $ assertBool "" $ Tree.isEmpty Tree.Leaf
    , testCase "Node isn't empty tree" $ assertBool "" $ not $ Tree.isEmpty (simpleTreeGenerator 1 1)
    ]

sizeTests :: TestTree
sizeTests =
  testGroup
    "size"
    [ testCase "empty tree"            $ 0  @=? Tree.size Tree.Leaf
    , testCase "1 element"             $ 1  @=? Tree.size (simpleTreeGenerator 1 1)
    , testCase "3 elements"            $ 3  @=? Tree.size treeWithTwoChild
    , testCase "17 repeated elements"  $ 17 @=? Tree.size simpleTreeWithRepeatedElement
    ]

findElementTests :: TestTree
findElementTests =
  testGroup
    "findElement"
    [ testCase "empty tree"             $ assertBool "" $ isLeft $ Tree.findElement Tree.Leaf (0 :: Integer)
    , testCase "find single element"    $ simpleTreeGenerator 1 1 @=? fromRight Tree.Leaf (Tree.findElement (simpleTreeGenerator 1 1) 1)
    , testCase "find one of 3 elements" $ simpleTreeGenerator 1 2 @=? fromRight Tree.Leaf (Tree.findElement treeWithTwoChild 2)
    ]

insertElementTests :: TestTree
insertElementTests =
  testGroup
    "insert"
    [ testCase "insert in empty tree"       $ simpleTreeGenerator 1 1 @=? Tree.insertElement Tree.Leaf 1
    , testCase "insert in tree wirh childs" $ treeWithTwoChild @=? Tree.insertElement (Tree.insertElement (simpleTreeGenerator 1 1) 0) 2
    , testCase "insert in repeated vertex"  $ simpleTreeGenerator 18 42 @=? Tree.insertElement simpleTreeWithRepeatedElement 42
    ]


fromListElementTests :: TestTree
fromListElementTests =
  testGroup
    "from list"
    [ testCase "empty tree"            $ (Tree.Leaf :: Tree.Tree Integer) @=? Tree.fromList []
    , testCase "tree with one element" $ simpleTreeGenerator 1 1 @=? Tree.fromList [1]
    , testCase "tree with 3 element"   $ treeWithTwoChild @=? Tree.fromList [0, 2, 1]
    , testCase "tree with 3 element"   $ treeWithTwoChild @=? Tree.fromList [2, 0, 1]
    , testCase "tree with 17 element"  $ simpleTreeWithRepeatedElement @=? Tree.fromList (replicate 17 (42 :: Integer)) 
    ]

removeElementTests :: TestTree
removeElementTests =
  testGroup
    "remove"
    [ testCase "remove in single element tree" $ 
        (Tree.Leaf :: (Tree.Tree Integer)) @=?
        fromRight (simpleTreeGenerator 2 2) (Tree.removeElement (simpleTreeGenerator 1 1) 1) 
    , testCase "remove in empty tree should fail" $ assertBool "" $ isLeft $ Tree.removeElement Tree.Leaf (0 :: Integer)
    , testCase "remove in repeated vertex" $
        simpleTreeGenerator 16 42 @=? fromRight Tree.Leaf (Tree.removeElement simpleTreeWithRepeatedElement 42)
    , testCase "remove in treeWith child" $
        Tree.Node 1 1 (simpleTreeGenerator 1 0) Tree.Leaf
        @=? fromRight (simpleTreeGenerator 3 3) (Tree.removeElement treeWithTwoChild 2)
    ]


propertyFoldableTest :: TestTree
propertyFoldableTest =
  SC.testProperty "toList . fromList = sort" $ \(list :: [Int]) -> (toList . Tree.fromList) list == sort list
