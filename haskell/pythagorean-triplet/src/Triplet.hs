module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = let [d, e, f] = sort [a, b, c]
        in d^2 + e^2 == f^2

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a, b, c) 

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = 
    [ (x, y, z) | x<-[minFactor..maxFactor], y<-[x..maxFactor], z<-[y..maxFactor], isPythagorean (x, y, z)]

