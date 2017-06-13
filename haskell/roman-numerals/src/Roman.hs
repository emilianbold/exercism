module Roman (numerals) where

import Prelude hiding (min, max)

letter :: Int -> Char
letter c = case c of
    1000 -> 'M'
    500 -> 'D'
    100 -> 'C'
    50 -> 'L'
    10 -> 'X'
    5 -> 'V'
    1 -> 'I'

numeral :: Int -> String
numeral n =
    let chunks = [900,800..100] ++ [90,80..10] ++ [9,8..1]
        thousands = replicate (n `div` 1000) 'M'
        num = map letter $ fst $ foldl (\(acc, v) d -> if v >= d then (acc ++ (roman d), v - d) else (acc, v)) ([], n `mod` 1000) chunks
    in thousands ++ num
 
roman :: Int -> [Int]
roman n 
    | n>=1 && n<=10     = (between 1 5 10) n 
    | n>=10 && n<=100   = (between 10 50 100) n 
    | n>=100 && n<=1000 = (between 100 500 1000) n

between :: Int -> Int -> Int -> Int -> [Int] 
between min middle max n
    | n == min              = [min]
    | n == max              = [max]
    | n == middle           = [middle]
    | n == (middle - min)   = [min, middle]
    | n == (max - min)      = [min, max]
    | n > middle            = middle : replicate ((n - middle) `div` min) min
    | otherwise             = replicate (n `div` min) min

numerals :: Integer -> Maybe String
numerals n
    | n<=0 || n>3000 = Nothing
    | otherwise      = Just $ numeral (fromIntegral n)
