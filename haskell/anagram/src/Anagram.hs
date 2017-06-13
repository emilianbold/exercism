module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)
import qualified Data.Set as Set (empty, filter, insert, map, toList)

data Anagram = Anagram { lower, text ::String }

instance Eq Anagram where
    a == b = lower a == lower b

instance Ord Anagram where
    compare a b = lower a `compare` lower b

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
        let
            word = map toLower xs
            wordid = sort word
            words = filter ((word /=) . (map toLower)) xss
            set = foldl (\set x -> Set.insert Anagram { lower = (map toLower x), text = x} set) Set.empty words
        in Set.toList $ Set.map text $ Set.filter ((wordid ==) . sort . lower) set
