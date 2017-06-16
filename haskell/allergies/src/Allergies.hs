module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum, Bounded)

allergies :: Int -> [Allergen]
allergies score = 
    map toEnum $ filter (testBit score) [0..maxa]
    where maxa = fromEnum (maxBound :: Allergen)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = testBit score $ fromEnum allergen
