module DNA (nucleotideCounts) where

import Data.Either
import Data.Map (Map, fromList, adjustWithKey)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts s =
    let emptyMap = Right $ fromList [('A', 0), ('C', 0), ('T', 0), ('G', 0)]
    in foldl process emptyMap s
    where
          process (Left map) _  = Left map
          process (Right map) key
            | key `elem` "ACTG" = Right $ adjustWithKey (\k -> (+1)) key map
            | otherwise         = Left "Error"  
