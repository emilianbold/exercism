module Robot (Robot, mkRobot, resetName, robotName) where

import Data.Char (chr, ord)
import System.Random
import Control.Concurrent.MVar

data Robot = Robot (MVar String)

mkRobot :: IO Robot
mkRobot = do
    gen <- newStdGen
    var <- newMVar (generateName gen)
    return (Robot var)

resetName :: Robot -> IO ()
resetName (Robot var) = do
    gen <- newStdGen
    swapMVar var (generateName gen)
    return ()

robotName :: Robot -> IO String
robotName (Robot var) = do
    readMVar var

generateName :: StdGen -> String
generateName g =
    let zero        = ord '0'
        (l1, g2)    = randomR (ord 'A', ord 'Z') g
        (l2, g3)    = randomR (ord 'A', ord 'Z') g2
        (n1, g4)    = randomR (0, 9) g3
        (n2, g5)    = randomR (0, 9) g4
        (n3, _)     = randomR (0, 9) g5
    in map chr [l1, l2, zero + n1, zero + n2, zero + n3]
