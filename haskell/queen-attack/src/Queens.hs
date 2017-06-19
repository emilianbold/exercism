module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = 
    concat [ square row col | row <- [0..7], col <- [0..7]]
    where square row col
            | is white row col = "W "
            | is black row col = "B "
            | col == 7         = "_\n"
            | otherwise        = "_ "
          is (Just (a,b)) c d
            | (a,b) == (c,d)   = True
          is _ _ _ = False

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2)
    | r1 == r2 = True
    | c1 == c2 = True
    | abs (r1 - r2) == abs(c1 - c2) = True
    | otherwise = False
