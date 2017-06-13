module Hamming (distance) where

import Data.Maybe

distance :: String -> String -> Maybe Int
distance x y = distance' x y 0

distance' :: String -> String -> Int -> Maybe Int
distance' [] [] acc = Just acc
distance' (x:xs) (y:ys) acc
    | x/=y      = distance' xs ys (acc + 1)
    | otherwise = distance' xs ys acc
distance' _ _ _ = Nothing 
