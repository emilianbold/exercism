module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Empty | LinkedList a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum (LinkedList x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr new Empty  

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _     = False 

new :: a -> LinkedList a -> LinkedList a
new x linkedList = LinkedList x linkedList

next :: LinkedList a -> LinkedList a
next (LinkedList _ n) = n 

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Empty = Empty
reverseLinkedList list = r2 list Empty 
    where r2 Empty list = list
          r2 (LinkedList x n) list = r2 n (new x list) 

toList :: LinkedList a -> [a]
toList Empty = []
toList (LinkedList a rest) = a : toList rest 
