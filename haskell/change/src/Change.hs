module Change (findFewestCoins) where

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins target _
    | target < 0   = Nothing
findFewestCoins target coins =
    backtrack target (reverse coins) [] Nothing

--stop worse solution recursion
backtrack _ _ current best@(Just b)
    | length current >= length b    = best

--current is the best solution
backtrack 0 _ current _ = Just current

--no solution possible witout coins
backtrack _ [] _ best = best

backtrack n coins@(x:xs) current best
    | n < x     =                backtrack n          xs current best
    | otherwise = let bestsome = backtrack (n - x) coins (x:current) best
                      bestzero = backtrack n          xs current bestsome
                  in bestzero
