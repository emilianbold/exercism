module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts s =
    Right $ makeMap $ foldl addPair (0, 0, 0, 0) (map toPair s)

makeMap (a, c, g, t) = fromList [('A', a), ('C', c), ('G', g), ('T', t)]
addPair (a1, c1, g1, t1) (a2, c2, g2, t2) = ((a1 + a2), (c1 + c2), (g1 + g2), (t1 + t2)) 

toPair 'A' = (1,0,0,0)
toPair 'C' = (0,1,0,0)
toPair 'G' = (0,0,1,0)
toPair 'T' = (0,0,0,1)
