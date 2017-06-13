module Phone_hrantzsch (number) where

import qualified Data.Char as C

number :: String -> Maybe String
number = check . clean
    where
        clean = (\(x:xs) -> case x of
                                '1' ->     xs
                                _   -> x : xs)
                . filter C.isDigit
        check xs
            | length xs /= 10             = Nothing
            | (xs !! 3) `elem` ['0', '1'] = Nothing
            | otherwise                   = Just xs

