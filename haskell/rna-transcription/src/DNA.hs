module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA [] = Just []
toRNA s
    | not(valid s) = Nothing
    | otherwise = Just (map transform s)
    where 
        valid s = all expected s
            where expected c = c `elem` "CGTA"

        transform x = case x of
                    'G' -> 'C'
                    'C' -> 'G'
                    'T' -> 'A'
                    'A' -> 'U'
