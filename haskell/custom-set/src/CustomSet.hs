module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data CustomSet a = Empty | CustomSet [a] deriving (Show)

instance (Eq a) => Eq (CustomSet a) where
    left == right = size left == size right && isSubsetOf left right

delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete x set = difference set (CustomSet [x]) 

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference Empty _ = Empty
difference (CustomSet list) setB = CustomSet $ filter (not . (`member` setB)) list 

empty :: CustomSet a
empty = Empty 

fromList :: (Eq a) => [a] -> CustomSet a
fromList = foldr insert empty

insert :: (Eq a) => a -> CustomSet a -> CustomSet a
insert x Empty = CustomSet [x]
insert x set@(CustomSet items)
    | x `elem` items    = set
    | otherwise         = CustomSet (x : items)

intersection :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (CustomSet list) setB = fromList $ filter (`member` setB) list

isDisjointFrom :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom Empty Empty = True
isDisjointFrom Empty _ = True
isDisjointFrom (CustomSet list) setB = all (not . (`member` setB)) list

isSubsetOf :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (`member` setB) (toList setA)

member :: (Eq a) => a -> CustomSet a -> Bool
member x Empty = False
member x (CustomSet list) = x `elem` list

null :: CustomSet a -> Bool
null Empty = True
null _ = False

size :: CustomSet a -> Int
size Empty = 0
size (CustomSet list) = length list

toList :: CustomSet a -> [a]
toList Empty = []
toList (CustomSet list) = list

union :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = foldr insert setA (toList setB)
