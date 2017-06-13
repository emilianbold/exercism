module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter c = score $ filter ((elem (toUpper c)) . snd) scoreMap
    where scoreMap = [(1, "AEIOULNRST"), (2, "DG"), (3, "BCMP"),
                      (4, "FHVWY"), (5, "K"), (8, "JX"), (10, "QZ")]
          
          score [] = 0
          score [(a,b)] = a


scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
