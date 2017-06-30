module Frequency (frequency) where

import Control.Parallel (par)
import Data.Char (toLower, isAlpha)
import Data.Map  (Map, empty, insertWith, unionWith)
import Data.Text (Text)
import qualified Data.Text as T

chunksOf :: Eq a => Int -> [a] -> [[a]]
chunksOf n list
    | rest == []    = [chunk]
    | otherwise     = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n list

countText :: Text -> Map Char Int
countText text = T.foldr count empty $ T.filter isAlpha text
    where count c m = insertWith (+) (toLower c) 1 m 

countTexts :: [Text] -> Map Char Int
countTexts texts = unionAll $ map countText texts

unionAll :: [Map Char Int] -> Map Char Int
unionAll = foldr (unionWith (+)) empty

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
    mapPar [] $ chunksOf workload texts
    where   (d, m)      = (length texts) `divMod` nWorkers 
            workload    = if m == 0 then d else d + 1 

mapPar :: [Map Char Int] -> [[Text]] -> Map Char Int
mapPar results [] = unionAll results
mapPar results (x:xs) = par evalX $ mapPar (evalX:results) xs
                        where evalX = countTexts x

