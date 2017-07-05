module Say (inEnglish) where

import Data.Maybe (fromJust)
import Data.List (find)

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0     = Nothing
    | n == 0    = Just "zero"
    | otherwise = Just $ unwords $ say n

say :: Integer -> [String]
say 0 = []
say n = sayRange (lookup' n ranges)
        where sayRange (Just (d, text)) = say (n `div` d) ++ [text] ++ say (n `mod` d)
              sayRange _ = sayWord (lookup' n pairs)
              sayWord (Just (v, text))
                | n == v    = [text]
                | otherwise = [text ++ "-" ++ (unwords (say (n-v)))] 

ranges = [(10^9, "billion"), (10^6, "million"), (10^3, "thousand"), (100, "hundred")]
pairs  = reverse [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"),
                    (8, "eight"), (9, "nine"), (10, "ten"), (11, "eleven"), (12, "twelve"), (13, "thirteen"),
                    (14, "fourteen"), (15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen"),
                    (19, "nineteen"), (20, "twenty"), (30, "thirty"), (40, "forty"), (50, "fifty"),
                    (60, "sixty"), (70, "seventy"), (80, "eighty"), (90, "ninety")]


lookup' :: Integer -> [(Integer, String)] -> Maybe (Integer, String)
lookup' n = find (\(r, _) -> n >= r)
