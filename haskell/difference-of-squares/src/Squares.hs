module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference a = squareOfSums a - sumOfSquares a

squareOfSums :: Integral a => a -> a
squareOfSums a = (a * (a + 1) `div` 2)^2

sumOfSquares :: Integral a => a -> a
sumOfSquares a = sum [ x * x | x <- [1..a]]
