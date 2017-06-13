module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor s = responseFor' (filter (not . isSpace) s)

responseFor' [] = "Fine. Be that way!"
responseFor' s
	| allCaps s 	= "Whoa, chill out!"
	| '?' == last s	= "Sure."
	| otherwise 	= "Whatever."
	where 
		allCaps s = any isAsciiUpper s && not (any isAsciiLower s) 
