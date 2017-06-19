module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]
    where sieve []      = []
          sieve (p:xs)  = p : (sieve $ filter (not . dividesBy) xs)
            where dividesBy u = u `mod` p == 0 
