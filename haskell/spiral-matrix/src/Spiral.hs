module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n
    | n < 1 = []
spiral 1 = [[1]]
spiral 2 = [[1,2],[4,3]]
spiral n = let length= 4 * n - 4
               inner = map (map (+length)) $ spiral (n - 2)
               left = [length, length - 1..length - (n - 2)]
               right = [n + 1..2 * n - 2]
               middle = zipWith (:) left $ zipWith (++) inner $ map (\c -> [c]) right
               top = [1..n]
               bottom = [3 * n - 2, 3 * n - 3..2 * n - 1]
            in top : (middle ++ [bottom])
