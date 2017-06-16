module Series (largestProduct) where

import Data.Char (isDigit, digitToInt)

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits
    | size > digitsLen          = Nothing
    | size < 0                  = Nothing
    | not $ all isDigit digits  = Nothing
    | size == 0                 = Just 1
    | size == digitsLen         = Just $ foldl (*) 1 numbers 
    | otherwise = Just $ fst $ foldl search (0, (0, [])) numbers 
    where
        digitsLen = length digits
        numbers = map (fromIntegral . digitToInt) digits 
        search (total, (n, partials)) num 
            | size == 1     = (max total num, (n, []))
            | n < size - 1  = (total, (n + 1, mulAll ++ [num]))
            | otherwise     = (max total $ head mulAll, (n, tail mulAll ++ [num]))
            where mulAll = map (*num) partials
