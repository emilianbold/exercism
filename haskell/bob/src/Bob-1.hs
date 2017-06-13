module Bob (responseFor) where

responseFor :: String -> String
responseFor s = responseFor' (trim s)

responseFor' [] = "Fine. Be that way!"
responseFor' s
	| allCaps s 	= "Whoa, chill out!"
	| '?' == last s	= "Sure."
	| otherwise 	= "Whatever."
	where 
		allCaps s = length [ c | c <- s, c `elem` ['A'..'Z']] /= 0 && [] == [ c | c <-s, c `elem` ['a'..'z']]  

trim :: String -> String
trim s = rtrim (ltrim s)
	where
		ltrim [] = []
		ltrim s@(x:xs)
			| whitespace x = ltrim xs
			| otherwise = s

		rtrim s=reverse (ltrim (reverse s))

		whitespace ' ' = True
		whitespace '\n' = True
		whitespace '\t' = True
		whitespace '\r' = True
		whitespace _ = False
