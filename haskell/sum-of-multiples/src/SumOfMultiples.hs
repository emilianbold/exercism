module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples (factor:rest) limit
	= multiple factor rest limit + sumOfMultiples rest limit

multiple :: Integer -> [Integer] -> Integer -> Integer 
multiple factor rest limit =
	sum (filter (not . dividesOther) factorMultiples)
	where
		n = (limit - 1) `div` factor
		factorMultiples = map (* factor) [1..n]
		dividesOther x = not $ null $ filter (==0) (map (mod x) rest)
	
