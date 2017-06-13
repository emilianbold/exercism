module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concatMap toChars $ toList legacyData
    where toChars (a, name) = [ (toLower x, a) | x <- name ]
    
