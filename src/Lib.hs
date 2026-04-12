module Lib (
    generateRandomPoint,
) where

import Point (Point (..), mul, normalize)
import Shape (Shape (Circle, Triangle))
import System.Random (randomRIO)

generateRandomPoint :: Shape -> IO Point
generateRandomPoint (Triangle p0 vr ur) = do
    xiv <- randomRIO (0 :: Double, 1)
    xiu <- randomRIO (0 :: Double, 1)
    let v = vr - p0
    let u = ur - p0
    return $
        if xiv + xiu <= 1
            then p0 + v `mul` xiv + u `mul` xiu
            else p0 + v `mul` (1 - xiv) + u `mul` (1 - xiu)
generateRandomPoint (Circle n p0 r) = do
    xiv <- randomRIO (0 :: Double, 1)
    xiu <- randomRIO (0 :: Double, 1)
    let v =
            if x n == 0 && y n == 0
                then Point 1 0 0
                else normalize $ Point (negate $ y n / sqrt (y n ** 2 + x n ** 2)) (x n / sqrt (y n ** 2 + x n ** 2)) 0
    let u = normalize $ n * v
    if (xiv - 0.5) ** 2 + (xiu - 0.5) ** 2 <= 0.25
        then return $ p0 - v `mul` r - u `mul` r + v `mul` (2 * r * xiv) + u `mul` (2 * r * xiu)
        else generateRandomPoint (Circle n p0 r)