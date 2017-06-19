module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | BST a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (BST _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (BST _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (BST a _ _) = Just a 

empty :: BST a
empty = Empty 

fromList :: Ord a => [a] -> BST a
fromList = foldl (\t x -> insert x t) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (BST a left right)
    | x <= a    = BST a (insert x left) right
    | otherwise = BST a left (insert x right)

singleton :: a -> BST a
singleton x = BST x Empty Empty 

toList :: BST a -> [a]
toList Empty = []
toList (BST a left right) = (toList left) ++ [a] ++ (toList right) 
