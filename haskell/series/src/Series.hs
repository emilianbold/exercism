module Series (slices) where

import Prelude hiding (length, filter, map)

import Data.Sequence
import Data.Char

slices :: Int -> Seq Char -> Seq (Seq Int)
slices n xs
    | n > length xs     = empty
    | n == 0            = singleton empty 
    | otherwise         = fst $ foldl process (empty, empty) xs
    where 
          process (acc, wip) c
            | isDigit c = (acc >< ready, baking)
            | otherwise = (acc, wip)
            where 
                  plusc s = s |> (digitToInt c)
                  list2 = (fmap plusc wip) |> (plusc empty)
                  ready = filter oklen list2
                  baking = filter (not . oklen) list2
                  oklen x = n == length x
