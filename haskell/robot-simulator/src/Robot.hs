module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing (Integer, Integer)
                deriving (Eq, Show) 

bearing :: Robot -> Bearing
bearing (Robot bearing _) = bearing 

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coord) = coord 

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot 

simulate :: Robot -> String -> Robot
simulate = foldl roboStep
    where roboStep (Robot North (x, y)) 'A' = Robot North (x, y+1)
          roboStep (Robot South (x, y)) 'A' = Robot South (x, y-1)
          roboStep (Robot West  (x, y)) 'A' = Robot West  (x-1, y)
          roboStep (Robot East  (x, y)) 'A' = Robot East  (x+1, y)
          roboStep (Robot bearing coord) 'L'= Robot (turnLeft bearing) coord
          roboStep (Robot bearing coord) 'R'= Robot (turnRight bearing) coord 

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North
