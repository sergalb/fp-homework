module Task2Tests
  ( task2Tests
  ) where

import Task2
import Test.Tasty
import Test.Tasty.HUnit

task2Tests :: TestTree
task2Tests = testGroup "Task2 tests" [sumTests, mulTests, subTests, natToIntegerTests, integerToNatTests, eqTests, ordTests]

sumTests :: TestTree
sumTests =
  testGroup
    "sum tests"
    [ testCase "0 + 0 = 0" $ assertEqual "" (Z `hw1Sum` Z) Z
    , testCase "2 + 2 = 4" $ assertEqual "" ((S $ S Z) `hw1Sum` (S $ S Z)) $ S $ S $ S $ S Z
    , testCase "0 + 2 = 2" $ assertEqual "" (Z `hw1Sum` (S $ S Z)) $ S $ S Z
    , testCase "2 + 0 = 2" $ assertEqual "" ((S $ S Z) `hw1Sum` Z) $ S $ S Z
    , testCase "1 + 7 = 8" $ assertEqual "" (S Z `hw1Sum` (S $ S $ S $ S $ S $ S $ S Z)) $ S $ S $ S $ S $ S $ S $ S $ S Z
    , testCase "3 + 4 = 7" $ assertEqual "" ((S $ S $ S Z) `hw1Sum` (S $ S $ S $ S Z)) $ S $ S $ S $ S $ S $ S $ S Z
    ]

mulTests :: TestTree
mulTests =
  testGroup
    "mul tests"
    [ testCase "0 * 0 = 0" $ assertEqual "" (Z `hw1Mul` Z) Z
    , testCase "2 * 2 = 4" $ assertEqual "" ((S $ S Z) `hw1Mul` (S $ S Z)) $ S $ S $ S $ S Z
    , testCase "0 * 2 = 0" $ assertEqual "" (Z `hw1Mul` (S $ S Z)) Z
    , testCase "2 * 0 = 0" $ assertEqual "" ((S $ S Z) `hw1Mul` Z) Z
    , testCase "1 * 3 = 3" $ assertEqual "" (S Z `hw1Mul` (S $ S $ S Z)) $ S $ S $ S Z
    , testCase "3 * 1 = 3" $ assertEqual "" ((S $ S $ S Z) `hw1Mul` S Z) $ S $ S $ S Z
    , testCase "3 * 2 = 6" $ assertEqual "" ((S $ S $ S Z) `hw1Mul` (S $ S Z)) $ S $ S $ S $ S $ S $ S Z
    ]

subTests :: TestTree
subTests =
  testGroup
    "sub tests"
    [ testCase "0 - 0 = 0" $ assertEqual "" (Z `hw1Sub` Z) Z
    , testCase "2 - 0 = 2" $ assertEqual "" ((S $ S Z) `hw1Sub` Z) $ S $ S Z
    , testCase "3 - 1 = 2" $ assertEqual "" ((S $ S $ S Z) `hw1Sub` S Z) $ S $ S Z
    , testCase "4 - 3 = 1" $ assertEqual "" ((S $ S $ S $ S Z) `hw1Sub` (S $S $ S Z)) $ S Z
    , testCase "0 - 2 = 0" $ assertEqual "" (Z `hw1Sub` (S $ S Z)) Z
    ]

natToIntegerTests :: TestTree
natToIntegerTests =
  testGroup
    "Nat to Integer tests"
    [ testCase "0 is 0" $ assertEqual "" 0 (natToInteger Z)
    , testCase "1 is 1" $ assertEqual "" 1 (natToInteger $ S Z)
    , testCase "2 is 2" $ assertEqual "" 2 (natToInteger $ S $ S Z)
    , testCase "3 is 3" $ assertEqual "" 3 (natToInteger $ S $ S $ S Z)
    , testCase "7 is 7" $ assertEqual "" 7 (natToInteger $ S $ S $ S $ S $ S $ S $ S Z)
    ]

integerToNatTests :: TestTree
integerToNatTests =
  testGroup
    "Integer to Nat tests"
    [ testCase "0 is 0" $ assertEqual "" (integerToNat 0) Z
    , testCase "1 is 1" $ assertEqual "" (integerToNat 1) (S Z)
    , testCase "2 is 2" $ assertEqual "" (integerToNat 2) (S $ S Z)
    , testCase "3 is 3" $ assertEqual "" (integerToNat 3) (S $ S $ S Z)
    , testCase "7 is 7" $ assertEqual "" (integerToNat 7) (S $ S $ S $ S $ S $ S $ S Z)
    ]

eqTests :: TestTree
eqTests =
  testGroup
    "Nat equation tests"
    [ testCase "0 = 0" $ Z                             @?= Z
    , testCase "1 = 1" $ S Z                           @?= S Z
    , testCase "2 = 2" $ (S $ S Z)                     @?= (S $ S Z)
    , testCase "3 = 3" $ (S $ S $ S Z)                 @?= (S $ S $ S Z)
    , testCase "7 = 7" $ (S $ S $ S $ S $ S $ S $ S Z) @?= (S $ S $ S $ S $ S $ S $ S Z)
    , testCase "2 /= 3" $ assertBool "" $ (S $ S Z) /= (S $ S $ S Z)
    ]

ordTests :: TestTree
ordTests =
  testGroup
    "Nat Ord tests"
    [ testCase "0 < 1" $ (Z `compare` S Z)         @?= LT
    , testCase "2 > 1" $ ((S $ S Z) `compare` S Z) @?= GT   
    ]

