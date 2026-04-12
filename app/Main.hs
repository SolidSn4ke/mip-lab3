module Main (main) where

import Lib (generateRandomVector)

main :: IO ()
main = mainLoop 1000

-- let p1 = Point{x = -55.93249116886787, y = 0.5122670391208857, z = -88.20254274701747}
-- let p2 = Point{x = 63.00667452378266, y = 10.576613653204625, z = -76.14096056959201}
-- let p3 = Point{x = -12.68575045737073, y = 68.52411714160763, z = 61.83869637244368}
-- let t = Triangle p1 p2 p3
-- p <- generateRandomVector
-- print p
-- print $ len p

mainLoop :: Int -> IO ()
mainLoop 0 = return ()
mainLoop n = do
    p <- generateRandomVector
    print p
    mainLoop $ n - 1