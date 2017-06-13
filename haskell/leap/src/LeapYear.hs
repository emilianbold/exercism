module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear y 
	| y `mod` 400 == 0 = True
	| y `mod` 4 == 0 = y `mod` 100 /= 0
	| otherwise = False
