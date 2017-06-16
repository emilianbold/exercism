module House (rhyme) where

rhyme :: String
rhyme = nicelines $ map nthstanza [1..length pieces]
    where nicelines []      = []
          nicelines [a]     = a
          nicelines (x:xs)  = x ++ "\n" ++ nicelines xs
          nthstanza n       = stanza $ reverse $ take n revpieces
          stanza seedwords  = "This is " ++ concatMap line seedwords 
          line (a, "")      = a ++ "\n" 
          line (a, b)       = a ++ "\nthat " ++ b ++ " "
          revpieces         = reverse pieces
          pieces = 
            [("the horse and the hound and the horn",
            "belonged to"), ("the farmer sowing his corn",
            "kept"), ("the rooster that crowed in the morn",
            "woke"), ("the priest all shaven and shorn",
            "married"), ("the man all tattered and torn",
            "kissed"), ("the maiden all forlorn",
            "milked"), ("the cow with the crumpled horn",
            "tossed"), ("the dog",
            "worried"), ("the cat",
            "killed"), ("the rat",
            "ate"), ("the malt",
            "lay in"), ("the house that Jack built.", "")]
