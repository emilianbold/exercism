module Prime (nth) where

import Data.Maybe (fromJust)

nth :: Int -> Maybe Integer
nth n
    | n < 1 = Nothing
nth 1 = Just 2
nth n = Just $ process [2] (n - 1) [3,5..]
    where process primes togo (k:xs)
            | togo == 0     = head primes
            | any ((==0) . (k `mod`)) primes = process primes togo xs
            | otherwise = process (k : primes) (togo - 1) xs

