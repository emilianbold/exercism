module Acronym (abbreviate) where

import Data.Char
import Data.List

abbreviate :: String -> String
abbreviate s = abbr ' ' s

abbr :: Char -> String -> String
abbr _ [] = []
abbr prev (x:xs)
    | (isSpace prev || isPunctuation prev) && isAlpha x = toUpper x : abbr x xs
    -- camelcase
    | isLower prev && isUpper x = x : abbr x xs  
    | otherwise = abbr x xs

