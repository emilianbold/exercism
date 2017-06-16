module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector, (!), empty)
import qualified Data.Vector as V (fromList, length, map, (++), foldl, splitAt)

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix v) 
    | V.length v == 0 = 0
    | otherwise       = V.length (v ! 0) 

column :: Int -> Matrix a -> Vector a
column x (Matrix v) = V.map (! x) v  

flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.foldl (V.++) empty v 

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ map V.fromList xss 

fromString :: Read a => String -> Matrix a
fromString xs =
    let rows = lines xs
    in fromList $ map (map read . words) rows

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape dimensions matrix = Matrix $ V.fromList $ split dimensions $ flatten matrix
    where split :: (Int, Int) -> Vector a -> [Vector a] 
          split (rows, cols) v
            | V.length v == rows    = [v]
            | otherwise             = vhead : split (rows, cols) vrest
            where (vhead, vrest) = V.splitAt cols v

row :: Int -> Matrix a -> Vector a
row x (Matrix v) = v ! x 

rows :: Matrix a -> Int
rows (Matrix v) = V.length v

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix) 

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ V.fromList $ map (\x -> column x matrix) [0..cols matrix - 1]
