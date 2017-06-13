module Acronym (abbreviate) where

import Data.Char
import Data.List

abbreviate :: String -> String
abbreviate s = map (toUpper . head) $ words $ allspaces $ splitCamelCase ' ' s

splitCamelCase :: Char -> String -> String
splitCamelCase _ [] = []
splitCamelCase prev (x:xs)
    -- inject a space for camelcase
    | isLower prev && isUpper x = ' ' : x : splitCamelCase x xs  
    | otherwise = x : splitCamelCase x xs

allspaces :: String -> String
allspaces s = map punctuationToSpace s
    where punctuationToSpace c
            | isPunctuation c = ' '
            | otherwise = c
