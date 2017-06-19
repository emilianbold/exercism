module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isDigit, toLower, ord, chr)

decode :: String -> String
decode text = map aToZ $ map toLower $ filter isAlphaNum text
    where aToZ c
            | isDigit c = c
            | otherwise = chr $ toFrom c
            where toFrom x = ord 'a' + ord 'z' - ord x
 
encode :: String -> String
encode = unwords . chunksOf 5 . decode 

chunksOf :: Eq a => Int -> [a] -> [[a]]
chunksOf n list
    | rest == []    = [chunk]
    | otherwise     = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n list
