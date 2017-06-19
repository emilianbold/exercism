module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired x = check (filter (`elem` "()[]{}") x) []
    where check [] [] = True
          check [] _  = False
          check (p:list) stack
            | isOpen p                  = check list (p : stack)
            | stack == []               = False
            | not $ pair (head stack) p = False
            | otherwise                 = check list (tail stack) 
          isOpen '(' = True
          isOpen '[' = True
          isOpen '{' = True
          isOpen _   = False
          pair '(' ')' = True
          pair '[' ']' = True
          pair '{' '}' = True
          pair _ _     = False

