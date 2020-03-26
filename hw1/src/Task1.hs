module Task1 (
  nextDay
 , afterDays
 , isWeekend
 , daysToParty
 , WeekDays (..)
  ) where

data WeekDays
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq WeekDays where
  (==) Monday Monday       = True
  (==) Tuesday Tuesday     = True
  (==) Wednesday Wednesday = True
  (==) Thursday Thursday   = True
  (==) Friday Friday       = True
  (==) Saturday Saturday   = True
  (==) Sunday Sunday       = True
  (==) _ _                 = False

nextDay :: WeekDays -> WeekDays
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: Int -> WeekDays -> WeekDays
afterDays 0 day = day
afterDays x day =
  if x > 0
    then afterDays (x - 1) (nextDay day)
    else error ("First argument should be non-negative, actualy: " ++ show x)

isWeekend :: WeekDays -> Bool
isWeekend Sunday   = True
isWeekend Saturday = True
isWeekend _        = False

daysToParty :: WeekDays -> Int
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1

