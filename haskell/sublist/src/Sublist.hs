module Sublist (Sublist(..), sublist) where

import Data.List(inits)

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist [] []   = Equal
sublist [] _    = Sublist
sublist _ []    = Superlist
sublist xs ys
    | length xs == length ys && xs == ys        = Equal
    | length xs < length ys && sublistOf xs ys  = Sublist
    | sublistOf ys xs                           = Superlist
    | otherwise                                 = Unequal


sublistOf [] _ = True
sublistOf _ [] = False
sublistOf x y@(_:ys)
    | x `starts` y  = True
    | otherwise     = sublistOf x ys

starts [] _ = True
starts _ [] = False
starts (x:xs) (y:ys)
    | x == y    = starts xs ys
    | otherwise = False
                 
