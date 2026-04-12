module Point (Point (..), dotp, len, mul, normalize) where

import Data.Function ((&))
import Test.QuickCheck.Gen (choose)
import Test.Tasty.QuickCheck (Arbitrary (..))

data Point = Point {x :: Double, y :: Double, z :: Double} deriving (Show, Eq)

instance Arbitrary Point where
    arbitrary = do
        a <- choose (-100 :: Double, 100)
        b <- choose (-100 :: Double, 100)
        c <- choose (-100 :: Double, 100)
        return $ Point a b c

instance Num Point where
    (+) p1 p2 = Point (x p1 + x p2) (y p1 + y p2) (z p1 + z p2)

    (-) p1 p2 = p1 + negate p2

    (*) p1 p2 = Point (y p1 * z p2 - z p1 * y p2) (z p1 * x p2 - x p1 * z p2) (x p1 * y p2 - y p1 * x p2)

    negate p = Point (-x p) (-y p) (-z p)

    abs p = Point (abs $ x p) (abs $ y p) (abs $ z p)

    signum _ = error "signum of point is not defined"

    fromInteger n = Point d d d
      where
        d = fromIntegral n

dotp :: Point -> Point -> Double
dotp p1 p2 = sum $ map (\f -> f p1 * f p2) [x, y, z]

len :: Point -> Double
len p = sqrt . sum $ map (\f -> f p ** 2) [x, y, z]

mul :: Point -> Double -> Point
mul p v = Point (x p * v) (y p * v) (z p * v)

normalize :: Point -> Point
normalize p = len p ** (-1) & mul p