module Pangram (isPangram) where

import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram s =
	['a'..'z'] == unique (letterBag s) ' '

letterBag :: String -> String
letterBag s = sort (map toLower (filter isLetter s))

unique :: String -> Char -> String
unique [] _ = []
unique (x:xs) prev
	| x == prev = unique xs x
	| otherwise = x : unique xs x
