module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number s = let number = filter isDigit s
    in
    case length number of
        11 -> if (number !! 0 == '1') then validate $ tail number else Nothing
        10 -> validate number
        otherwise -> Nothing
    where validate number
            | twoToNine (number !! 0) && twoToNine (number !! 3) = Just number
            | otherwise = Nothing
          twoToNine x = x `elem` ['2'..'9']
