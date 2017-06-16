module Base (rebase) where

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1        = Nothing
    | outputBase <= 1       = Nothing
    | not validDigits       = Nothing
    | inputDigits == []     = Just []
    | all (==0) inputDigits = Just []
    | otherwise             = Just $ reverse $ toBase outputBase $ fromBase inputBase inputDigits
    where fromBase b digits     = foldl (\acc x -> acc * b + x) 0 digits
          toBase b n
            | n < b             = [n]
            | otherwise         = (n `mod` b) : toBase b (n `div` b)
          baseDigit b x         = (x >= 0) && (x < b)
          validDigits           = all (baseDigit inputBase) inputDigits
