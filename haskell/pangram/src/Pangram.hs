module Pangram (isPangram) where

import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram s = 26 == length (group $ sort $ map toLower $ filter isLetter s)
