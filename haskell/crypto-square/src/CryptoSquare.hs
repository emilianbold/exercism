module CryptoSquare (encode) where

import Data.Maybe (mapMaybe)
import Data.Char (isAlphaNum, toLower)

encode :: String -> String
encode xs = unwords $ byCols $ squareOf $ map toLower $ filter isAlphaNum xs
    where squareOf s = chunksOf (ceiling $ sqrt $ fromIntegral $ length s) s

byCols :: [[a]] -> [[a]]
byCols [] = []
byCols rows@(x:xs) = map col [0..length x -1]
    where col n = mapMaybe (\row -> if n >= length row then Nothing else Just $ row !! n) rows

chunksOf :: Eq a => Int -> [a] -> [[a]]
chunksOf n list
    | rest == []    = [chunk]
    | otherwise     = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n list
