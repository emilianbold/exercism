module Raindrops (convert) where

convert :: Int -> String
convert n =
    let s = concatMap raindrop [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    in case s of
        ""          -> show n
        otherwise   -> s
    where raindrop (d, text)
            | n `rem` d ==0 = text 
            | otherwise     = ""
