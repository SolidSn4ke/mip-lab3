module Main (main) where

import Lib
import Point
import Shape

main :: IO ()
main = do
    let p0 = Point 1 0 0
    let v = Point 1 0 5
    let u = Point 1 5 0
    let t = Triangle p0 v u
    p <- generateRandomPoint t
    print p