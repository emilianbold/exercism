module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n
    | any (not . digitOrSpace) n = False
    | otherwise = valid (filter (/=' ') n)
    where digitOrSpace ' ' = True
          digitOrSpace c   = isDigit c
          valid []  = False
          valid [x] = False
          valid v   = (luhnOf (map digitToInt v)) `mod` 10 == 0
          luhnOf numbers = sum $ double2nd $ reverse numbers
                where double2nd [] = []
                      double2nd [a] = [a]
                      double2nd (x:y:xs) = x : twice y : double2nd xs
                      twice y
                        | 2 * y < 10    = 2 * y
                        | otherwise     = 2 * y - 9 
