-- Cameron Wong

-- Implementation of the problems here:
--    http://courses.cs.washington.edu/courses/cse341/17wi/homeworks/hw1.pdf
-- in (mostly) point-free style as practice using arrows and lenses
--
-- I would do it in SML, but, like... Haskell.

-- Realistically, we would define Month as a data enum, but I want to stick to
-- the actual assignment spec.

import Control.Monad
import Control.Lens
import Control.Lens.Tuple
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ix

-- data and types
type Day = Int
type Month = Int
type Year = Int
type Date = (Day, Month, Year)

months = [
    "January"
  , "February"
  , "March"
  , "April"
  , "May"
  , "June"
  , "July"
  , "August"
  , "September"
  , "October"
  , "November"
  , "December"]

totalDays :: [Int]
totalDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- utility
untuplify3 :: (Int, Int, Int) -> [Int]
--untuplify3 (x,y,z) = [x,y,z]
untuplify3 = flip map (map (flip (^.)) [_1, _2, _3]) . flip ($)

mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 = join (***)

swap (x, y) = (y, x)
swap21 (x, y, z) = (y, x, z)
swap31 (x, y, z) = (z, y, x)

-- problem 1
isOlder :: Date -> Date -> Bool
isOlder = curry (uncurry (<) . mapT2 swap31)

-- problem 2
numberInMonth :: [Date] -> Month -> Int
numberInMonth = curry $ length . uncurry datesInMonth

-- problem 3
numberInMonths :: [Date] -> [Month] -> Int
--numberInMonths ds = sum . map (numberInMonth ds)
numberInMonths = curry $ sum . app . (first $ map . numberInMonth)

-- problem 4
datesInMonth :: [Date] -> Month -> [Date]
--datesInMonth ds m = filter (^._2 >>> (==) m) ds
datesInMonth = curry $ zipRepeat >>> filter (app . makeArrow) >>> map (^._1)
  where zipRepeat = uncurry zip . second repeat
        makeArrow = swap . ((^._2) *** (==))

-- problem 5
datesInMonths :: [Date] -> [Month] -> [Date]
datesInMonths = curry $ join . app . (first $ map . datesInMonth)

-- problem 6
getNth :: [a] -> Int -> a
getNth = curry $ uncurry (!!) . second (subtract 1)

-- problem 7
dateToString :: Date -> String
--dateToString d m y = intercalate "-" [getNth months m, show d, show y]
dateToString = swap21 >>> untuplify3 >>> (head &&& tail) >>> format
  where display = (getNth months *** map show)
        format = display >>> uncurry (:) >>> intercalate "-"

-- problem 8
nBeforeSum :: Int -> [Int] -> Int
--nBeforeSum n = length . takeWhile (< n) . scanl1 (+)
-- We could use flip (<) instead of (>), but they're the same thing so wtv
nBeforeSum = curry $ length . app . (takeWhile . (>) *** scanl1 (+))

-- problem 9
whatMonth :: Int -> Month
whatMonth = (+) 1 . flip nBeforeSum totalDays

-- problem 10
monthRange :: Int -> Int -> [Month]
monthRange = curry $ map whatMonth . range

-- problem 11
oldest :: [Date] -> Maybe Date
-- I legitimately have no idea how to do this without pattern matching. Maybe
-- using some fmap magic?
oldest [] = Nothing
oldest ds = Just $ swap31 . minimum $ map swap31 ds

-- problem 12
cumulativeSum = scanl (+) 0

-- problem 13
numberInMonthsChal = curry $ uncurry numberInMonths . second nub
datesInMonthsChal = curry $ uncurry datesInMonths . second nub

-- problem 14
-- TODO: Leap Years
reasonable :: Date -> Bool
reasonable =
  let validYr = (> 0) . (^._3)
      validMon :: Date -> Bool
      validMon = uncurry (&&) . ((> 0) &&& (<= 12)) . (^._2)
      -- thanks lazy eval for making sure we don't need to check month in
      -- this function as well
      dayInMonth :: Date -> Bool
      dayInMonth = app . ((>=) . getNth totalDays . (^._2) &&& (^._1))
      validDay :: Date -> Bool
      validDay = uncurry (&&) . (dayInMonth &&& ((>0) . (^._1)))
  in foldl1 (&&) . flip map [validYr, validMon, validDay] . flip ($)


