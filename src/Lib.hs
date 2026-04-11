module Lib (
    generateRandomPoint,
) where

import Point (Point, mul)
import Shape (Shape (Triangle))
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
generateRandomPoint _ = error "not implemented"