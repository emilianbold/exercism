module OCR (convert) where

import qualified Data.List as List (elemIndex, zip4)

digits = chunksOf 3 $ zip4' $ tail $ lines "\n\
\ _     _  _     _  _  _  _  _  _ \n\
\| |  | _| _||_||_ |_   ||_||_|| |\n\ 
\|_|  ||_  _|  | _||_|  ||_| _||_|\n\
\                                 "

chunksOf :: Eq a => Int -> [a] -> [[a]]
chunksOf n list
    | rest == []    = [chunk]
    | otherwise     = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n list

join sep list = foldl (\s x -> if s == "" then x else s ++ sep ++ x) "" list

zip4' [a,b,c,d] = List.zip4 a b c d

convert :: String -> String
convert xs = join "," $ map printerLine $ chunksOf 4 (lines xs)
    where printerLine :: [String] -> String
          printerLine text = concatMap numberOf $ map (\n -> List.elemIndex n digits) $ chunksOf 3 $ zip4' text
          numberOf Nothing = "?"
          numberOf (Just x) = show x
