module HW1.T1
  ( Day (..),
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
  )
where

import           GHC.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show)

nextDay :: Day -> Day
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day =
  afterDays (n - 1) $ nextDay day

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Natural
daysToParty day = daysToPartyHelper day 0
  where
    daysToPartyHelper :: Day -> Natural -> Natural
    daysToPartyHelper Friday n = n
    daysToPartyHelper day' n   = daysToPartyHelper (nextDay day') $ n + 1
