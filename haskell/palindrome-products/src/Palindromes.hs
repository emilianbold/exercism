module Palindromes (largestPalindrome, smallestPalindrome) where
{-
Instead of trying all product pairs for palindromes, build the palindromes and check for divisors.

A palindrome 'seed' is half of palindrome. It may be odd or even.

An odd palindrome 12321 has the seed 123.
An even palindrom 123321 has the same seed 123.
-}

import Data.Maybe (fromJust)

largestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = let (seed, odd) = palSeed (<=) (maxFactor^2) decrement 
        in fromJust $ nextPalindrome seed odd (<minFactor^2) adjustDown minFactor maxFactor

decrement c = c - 1

-- adjust the palindrom seed down
adjustDown :: Integer -> Bool -> (Integer, Bool) 
adjustDown seed odd = adjustDown' seed (decrement seed) odd

adjustDown' :: Integer -> Integer -> Bool -> (Integer, Bool)
-- 1
adjustDown' 1 _ True = error "Finished palindromes"
-- 1|1 -> 9
adjustDown' 1 _ False = (9, True)
adjustDown' seed newseed odd
    | digits seed  == digits newseed  = (newseed, odd)
-- 10|1 -> 9|9 
adjustDown' seed newstep True   = (newstep, False)
-- 10|01 -> 99|9 -- corner case, here newstep == 10 - 1 
adjustDown' seed newstep False  = (newstep * 10 + 9, True)

smallestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = let (seed, odd) = palSeed (>=) (minFactor^2) (+1)
        in fromJust $ nextPalindrome seed odd (>maxFactor^2) adjustUp minFactor maxFactor

-- adjust a palindrome seed up
adjustUp :: Integer -> Bool -> (Integer, Bool)
adjustUp seed odd = adjustUp' seed (seed + 1) odd

adjustUp' seed newstep odd
    | digits seed == digits newstep  = (newstep, odd)
-- 99|9 -> 10|01 -- corner case, here newstep == 99 + 1
adjustUp' _ newstep True            = (newstep `div` 10, False)
-- 99|99 -> 100|01 
adjustUp' _ newstep False           = (newstep, True)

-- extract the first seed from an interval while respecting the boundary (lower / upper)
palSeed :: (Integer -> Integer -> Bool) -> Integer -> (Integer -> Integer) -> (Integer, Bool)
palSeed boundary n step =
            let k = digits n
                odd = k `mod` 2 == 1
                seedDigits = (k - (k `mod` 2)) `div` 2
                seed = n `div` (10^seedDigits)
                firstPal = palindrome seed odd
             in if boundary firstPal n
                    then (seed, odd)
                    -- todo: I suspect `step seed` cannot change `odd` since the remainder of the seed is smaller than 10^(remaining digits) 
                    else (step seed, odd)

-- search for the palindrom in a direction (given by `adjust`) based on the `seed`. Stop when reaching the `limit`.
nextPalindrome :: Integer -> Bool -> (Integer -> Bool) -> (Integer -> Bool -> (Integer, Bool)) -> Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
nextPalindrome seed odd limit adjust minFactor maxFactor =
    let
        pal             = palindrome seed odd
        divisors        = filter (\(a, b) -> (minFactor <= b) && (b <= maxFactor)) $ map (\d -> (d, pal `div` d)) $ filter (\d -> pal `mod` d == 0) [minFactor..maxFactor]
    in
        if limit pal 
            then Nothing
            else if 0/= length divisors then
                Just (pal, divisors)
            else
                nextPalindrome nSeed nOdd limit adjust minFactor maxFactor 
    where (nSeed, nOdd)   = adjust seed odd

-- count digits in a positive number
digits :: Integer -> Integer
digits n
    | n <= 0 = error $ "Only positive numbers, got " ++ (show n)
digits n = digits' n
    where digits' 0 = 0
          digits' n = 1 + digits' (n `div` 10)

-- Build palindrome from an odd/even seed.
-- Odd  seed 123 == 12321
-- Even seed 123 == 123321
palindrome :: Integer -> Bool -> Integer
palindrome seed odd = palindrome' seed seed odd
    where
        palindrome' seed 0 _         = seed
        palindrome' seed rest True   = palindrome' seed (rest `div` 10) False
        palindrome' seed rest False  = palindrome' (seed * 10 + rest `mod` 10) (rest `div` 10) False
