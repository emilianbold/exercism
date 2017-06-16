module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n
    | n < 0 = [] 
rows 1 = [[1]]
rows n = prevRows ++ [1 : takeTwo (last prevRows)]
    where prevRows = rows (n - 1)
          takeTwo [1] = [1]
          takeTwo (x:xs@(y:_)) = (x + y) : takeTwo xs
