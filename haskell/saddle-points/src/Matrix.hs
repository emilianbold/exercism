module Matrix (saddlePoints) where

import Data.Array (Array, indices, elems, assocs, array, (!))
import Data.Ix

import Data.List (groupBy, maximumBy, minimumBy, intersect)

saddlePoints :: (Ix i, Ord e) => Array i e -> [i]
saddlePoints matrix =
    let rows = byRows $ indices matrix
        cols = byColumns rows
        maxrows = concatMap (allWhere maximumBy matrix) rows
        mincols = concatMap (allWhere minimumBy matrix) cols
    in intersect maxrows mincols

-- all min / max from list
allWhere :: (Ix i, Ord e) => ((i -> i -> Ordering) -> [i] -> i) -> Array i e -> [i] -> [i]
allWhere f matrix idx = filter (\z -> valm == matrix ! z) idx 
    where m = f compareValFor idx 
          valm = matrix ! m
          compareValFor a b = compare (matrix ! a) (matrix ! b)

byColumns :: [[i]] -> [[i]]
byColumns [] = []
byColumns rows@(x:xs) =
    let n = length x
    in map getCol [0..n-1]
    where getCol c = map (!!c) rows

-- figure out rows based on closeness
byRows :: Ix i => [i] -> [[i]]
byRows indices = incGroupBy isNeighbour $ indices
    where isNeighbour x y = 2 == length (range (x, y))

-- incremental group by
incGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
incGroupBy f [] = []
incGroupBy f (x:xs) = incGroupBy' f xs x [[x]]

incGroupBy' :: (a -> a -> Bool) -> [a] -> a -> [[a]] -> [[a]]
incGroupBy' f [] _ collect = reverse $ map reverse collect
incGroupBy' f (x:xs) prev collect
    -- add to working sublist
    | f prev x  = incGroupBy' f xs x ((x : head collect) : (tail collect))
    -- start new working sublist
    | otherwise = incGroupBy' f xs x ([x] : collect)

