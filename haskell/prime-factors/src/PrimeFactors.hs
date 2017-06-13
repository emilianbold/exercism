module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n =
    step n 2
    where
          maxn               = floor $ sqrt $ fromIntegral n
          step 1 d           = []
          step v d
            | v `mod` d == 0 = d : step (v `div` d) d
            | d > maxn      = [v]
          step v 2           = step v 3
          step v d           = step v (d + 2)
