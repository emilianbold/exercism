module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Eq a, Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = if valid a b c then triangleType' a b c else Illegal 

valid :: (Num a, Ord a) => a -> a -> a -> Bool
valid a b c = abs(a - b) < c && c < (a + b) && a > 0 && b > 0 && c > 0 

triangleType' :: (Eq a) => a -> a -> a -> TriangleType
triangleType' a b c
    | a == b && b == c  = Equilateral
    | a == b            = Isosceles
    | b == c            = Isosceles
    | a == c            = Isosceles
    | otherwise         = Scalene
