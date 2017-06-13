module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList, findWithDefault)
import Data.List (zip, sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultGarden :: String -> Map String [Plant]
defaultGarden plants = garden ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"] plants 

garden :: [String] -> String -> Map String [Plant]
garden students plants =
    let [frontRow, backRow] = lines plants
        front = map charToPlant frontRow
        back  = map charToPlant backRow
        -- [[plant1,..,plant4]]
        pots  = zipWith (++) (everyTwo front) (everyTwo back) 
    in fromList $ zip (sort students) pots
    where charToPlant 'C' = Clover
          charToPlant 'G' = Grass
          charToPlant 'R' = Radishes
          charToPlant 'V' = Violets
          everyTwo []       = []
          everyTwo (x:y:xs) = [x,y] : everyTwo xs

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = findWithDefault [] student garden 
