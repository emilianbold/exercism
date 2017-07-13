module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (find)
import Data.Maybe

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum)

data Color = Red | Green | Ivory | Blue | Yellow deriving (Eq, Enum, Show)

data Animal = Dog | Snail | Fox | Horse | Zebra deriving (Eq, Enum, Show)

data Drink = Coffee | Milk | Juice | Tea | Water deriving (Eq, Enum, Show)

data Smokes = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Eq, Enum, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = extractSolution $ head $ solve2 []

data Tuple = Tuple { color :: Color, resident :: Resident, animal :: Animal, drink :: Drink, smoke :: Smokes} deriving (Eq, Show)

solve2 :: [Tuple] -> [[Tuple]]
solve2 list
    | length list == 5 = [list]
    | otherwise = let 
            attempts = [tuple |
                c <- enumFrom Red,
                r <- enumFrom Englishman,
                a <- enumFrom Dog,
                d <- enumFrom Coffee,
                s <- enumFrom OldGold,
                let tuple = Tuple c r a d s,
                -- unique
                not (any ((c==) . color) list),
                not (any ((r==) . resident) list),
                not (any ((a==) . animal) list),
                not (any ((d==) . drink) list),
                not (any ((s==) . smoke) list),
                --1
                length list < 5,
                (r == Englishman) == (c == Red),
                (r == Spaniard) == (a == Dog),
                (d == Coffee) == (c == Green),
                --5
                (r == Ukrainian) == (d == Tea),
                (c == Green) == (list /=[] && color (head list) == Ivory), 
                (s == OldGold) == (a == Snail),
                (s == Kools) == (c == Yellow),
                (d == Milk) == ((length list) == 2),
                --10
                (r == Norwegian) == (list == []),
                nextTo ((Chesterfields ==) . smoke) ((Fox ==) . animal) tuple,
                nextTo ((Kools ==) . smoke) ((Horse ==) . animal) tuple,
                (s == LuckyStrike) == (d == Juice),
                (r == Japanese) == (s == Parliaments),
                --15
                nextTo ((Norwegian ==) . resident)  ((Blue ==) . color) tuple
                ]
                in concatMap (\a -> solve2 (a : list)) attempts
                where near p
                        -- if the predicate matches, it must be the previous house
                        | any p list = p (head list)
                        | otherwise  = True
                      nextTo :: (Tuple -> Bool) -> (Tuple -> Bool) -> Tuple -> Bool
                      nextTo p1 p2 tuple
                        | p1 tuple  = not (p2 tuple) && (near p2)
                        | p2 tuple  = not (p1 tuple) && (near p1)
                        | otherwise = True


extractSolution list = Solution (who ((Water ==) . drink)) (who ((Zebra ==) . animal))
                        where who p = (resident (fromJust (find p list)))
