module Minesweeper (annotate) where

toC Nothing             = '*' 
toC (Just 0)            = ' ' 
toC (Just x)
    | x >= 0 && x < 10  = head $ show x

toS :: [(Maybe Int)] -> String
toS list = map toC list

zeros n = replicate n (Just 0)

add2 :: [(Maybe Int)] -> [(Maybe Int)] -> [(Maybe Int)]
add2 = zipWith (\a b -> (+) <$> a <*> b)

-- right influence of '*'
right x = right' 0 x
right' _ []             = []
right' plus (' ':xs)    = Just plus : right' 0 xs
right' plus ('*':xs)    = Just plus : right' 1 xs

-- top-left influence of '*'
left x = tail $ left' x Nothing
left' [] prev = [prev]
left' (' ':xs) prev = prev : left' xs (Just 0)
left' ('*':xs) prev = ((1+) <$> prev) : left' xs (Just 1)

star line = map toN line
    where toN '*' = Nothing
          toN ' ' = Just 0

central line = add2 (star line) (top line)

top line     = add2 (right line) (left line) 

annotate :: [String] -> [String]
annotate []             = []
annotate board@(x:_)    = let (out, (last, _)) = foldl process ([],(zeros len, zeros len)) board 
                          in tail $ reverse $ toS last : out
                          where len = length x

process :: ([String], ([(Maybe Int)],[(Maybe Int)])) -> String -> ([String], ([(Maybe Int)],[(Maybe Int)]))

process (out, (prev, bottom)) line = let t = top line
                                         c = central line
                                         b = t
                                     in (toS (add2 prev t) : out, (add2 c bottom, b))

