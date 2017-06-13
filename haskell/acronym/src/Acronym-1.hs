module Acronym (abbreviate) where

import Data.Char
import Data.List

abbreviate :: String -> String
abbreviate s = map (toUpper . head) $ splitWords $ splitCamelCase ' ' s

splitCamelCase :: Char -> String -> String
splitCamelCase _ [] = []
splitCamelCase prev (x:xs)
    -- inject a space for camelcase
    | isLower prev && isUpper x = ' ' : x : splitCamelCase x xs  
    | otherwise = x : splitCamelCase x xs

splitWords :: String -> [String]
splitWords s = filter (not . sep . head) (groupBy separator s)
    where separator a b
            | sep a && sep b = True
            | sep a || sep b = False
            | otherwise = True

          sep c = isSpace c || isPunctuation c
