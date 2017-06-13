module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode s = decode2 0 s

decode2 :: Int -> String -> String
decode2 _ []		= []
decode2 num (x:xs)
	| isDigit x	= decode2 (num * 10 + digitToInt x) xs
	| num == 0	= x : decode2 0 xs
	| otherwise 	= (replicate num x) ++ decode2 0 xs

encode :: String -> String
encode [] = []
encode (x:xs) = encode2 1 x xs


encode2 :: Int -> Char -> String -> String
encode2 1 prev [] = [prev]
encode2 num prev [] = (show num) ++ [prev]
encode2 num prev (x:xs)
	| prev == x 	= encode2 (num + 1) prev xs
	| num == 1	= prev : encode2 1 x xs
	| otherwise	= (show num) ++ (prev : encode2 1 x xs)
