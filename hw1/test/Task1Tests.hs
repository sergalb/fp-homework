module Task1Tests where

import Task1
import Test.Tasty
import Test.Tasty.HUnit

task1Tests :: TestTree
task1Tests = testGroup "Task1 tests" [nextDayTests, afterDaysTests, isWeekendTest, daysToPartyTest]

nextDayTests :: TestTree
nextDayTests =
  testGroup
    "nextDay"
    [ testCase "nextDay Monday == Tuesday" $ assertEqual "not Tuesday after Monday" (nextDay Monday) Tuesday
    , testCase "nextDay Sunday == Monday"  $ assertEqual "not Monday after Sunday" (nextDay Sunday) Monday
    ]

afterDaysTests :: TestTree
afterDaysTests =
  testGroup
    "afterDays"
    [ testCase "nextDay Monday == Tuesday"     $ assertEqual "not Tuesday after Monday" (afterDays 1 Monday) Tuesday
    , testCase "nextDay Sunday == Monday"      $ assertEqual "not Monday after Sunday" (afterDays 1 Sunday) Monday
    , testCase "after 7 day Monday == Monday"  $ assertEqual "" (afterDays 7 Monday) Monday
    , testCase "after 2 day Wensday == Friday" $ assertEqual "" (afterDays 2 Wednesday) Friday
    ]

isWeekendTest :: TestTree
isWeekendTest =
  testGroup
    "isWeekend"
    [ testCase "Monday isn't weekend"    $ assertBool "" $ not $ isWeekend Monday
    , testCase "Tuesday isn't weekend"   $ assertBool "" $ not $ isWeekend Tuesday
    , testCase "Wednesday isn't weekend" $ assertBool "" $ not $ isWeekend Wednesday
    , testCase "Thursday isn't weekend"  $ assertBool "" $ not $ isWeekend Thursday
    , testCase "Friday isn't weekend"    $ assertBool "" $ not $ isWeekend Friday
    , testCase "Saturday is weekend"     $ assertBool "" $ isWeekend Saturday
    , testCase "Sunday is weekend"       $ assertBool "" $ isWeekend Sunday
    ]

daysToPartyTest :: TestTree
daysToPartyTest =
  testGroup
    "daysToParty"
    [ testCase "from Monday 4 days to party"    $ assertEqual "" (daysToParty Monday) 4
    , testCase "from Tuesday 3 days to party"   $ assertEqual "" (daysToParty Tuesday) 3
    , testCase "from Wednesday 2 days to party" $ assertEqual "" (daysToParty Wednesday) 2
    , testCase "from Thursday 1 days to party"  $ assertEqual "" (daysToParty Thursday) 1
    , testCase "from Friday 0 days to party"    $ assertEqual "" (daysToParty Friday) 0
    , testCase "from Saturday 5 days to party"  $ assertEqual "" (daysToParty Saturday) 6
    , testCase "from Sunday 6 days to party"    $ assertEqual "" (daysToParty Sunday) 5
    ]
