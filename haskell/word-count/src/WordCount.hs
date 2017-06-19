module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import qualified Data.Map as Map

wordCount :: String -> [(String, Int)]
wordCount xs = Map.toList $ foldl collect Map.empty $ map unquote $ words $ cleanup xs
    where
          collect m w = Map.insertWith (+) w 1 m 
          cleanup xs = map toLower $ map (\c -> if separator c then ' ' else c) xs
          separator '\'' = False
          separator c = not (isAlphaNum c)
          unquote ('\'' : xs@(_:_))
            | last xs == '\'' = init xs
          unquote x = x
