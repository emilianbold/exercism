module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
            deriving (Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
            deriving (Enum)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Teenth weekday year month = nextOf weekday year month 13

meetupDay First weekday year month = nextOf weekday year month 1

meetupDay Last weekday year month = prevOf weekday year month $ gregorianMonthLength year month

meetupDay nth weekday year month = addDays 7 $ meetupDay (pred nth) weekday year month

prevOf :: Weekday -> Integer -> Int -> Int -> Day
prevOf = dayOf (-1)

nextOf :: Weekday -> Integer -> Int -> Int -> Day
nextOf = dayOf 1

dayOf :: Int -> Weekday -> Integer -> Int-> Int -> Day
dayOf direction weekday year month dayOfMonth =
    let day                 = fromGregorian year month dayOfMonth 
        (_, _, dayOfWeek)   = toWeekDate day
        offset              = (7 + direction * (fromEnum weekday - dayOfWeek + 1)) `mod` 7
    in addDays (toInteger (direction * offset)) day
