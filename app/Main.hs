module Main (main) where

import Lib
import Point
import Shape

main :: IO ()
main = do
    let p0 = Point 0 0 0
    let v = Point 5 0 0
    let u = Point 0 5 0
    let t = Triangle p0 v u
    p <- generateRandomPoint t
    print p