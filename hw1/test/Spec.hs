import Test.Tasty

import Block2.Task2Tests
import Block3.Monoids.Task1Tests
import Task1Tests (task1Tests)
import Task2Tests (task2Tests)
import TreeTests (treeTests)

main :: IO ()
main =
  defaultMain $
  testGroup "tests for hw 1" [task1Tests, task2Tests, treeTests, block2Task2Tests, block3Task1Tests, block3Task1Tests]
