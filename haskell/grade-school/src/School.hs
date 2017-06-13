module School (School, add, empty, grade, sorted) where

import Data.Maybe
import Data.Map (Map, insertWith, toAscList)
import qualified Data.Map as M (lookup, empty) 
import Data.List (sort)

data School = School (Map Int [String]) 

add :: Int -> String -> School -> School
add gradeNum student (School grades) = School $ insertWith (\newv oldv -> sort $ newv ++ oldv) gradeNum [student] grades 

empty :: School
empty = School M.empty 

grade :: Int -> School -> [String]
grade gradeNum (School grades) = fromMaybe [] (M.lookup gradeNum grades)

sorted :: School -> [(Int, [String])]
sorted (School grades) = toAscList grades 
