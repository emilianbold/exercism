module PigLatin (translate) where

translate :: String -> String
translate = 
    unwords . (map pig) . words
    where pig w@(h:_)
            | h `elem` "aeiou" = w ++ "ay"
          pig w@('y':'t':_)    = w ++ "ay"
          pig w@('x':'r':_)    = w ++ "ay"
          pig ('c':'h':r)      = r ++ "chay"
          pig ('q':'u':r)      = r ++ "quay"
          pig (h:'q':'u':r)    = r ++ [h] ++ "quay"
          pig ('t':'h':'r':r)  = r ++ "thray"
          pig ('t':'h':r)      = r ++ "thay"
          pig ('s':'c':'h':r)  = r ++ "schay"
          pig (h:r)            = r ++ [h] ++ "ay"
