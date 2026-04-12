module Shape (Shape (..), contains, perimeter) where

import Point
import Test.Tasty.QuickCheck (Arbitrary (..), oneof)

data Shape = Triangle Point Point Point | Circle Point Point Double deriving (Show)

instance Arbitrary Shape where
    arbitrary =
        oneof
            [ Circle <$> arbitrary <*> arbitrary <*> arbitrary
            , Triangle <$> arbitrary <*> arbitrary <*> arbitrary
            ]

perimeter :: Shape -> Double
perimeter (Triangle p1 p2 p3) =
    let
        a = len $ p1 - p2
        b = len $ p2 - p3
        c = len $ p3 - p1
     in
        a + b + c
perimeter (Circle _ _ r) = 2 * pi * r

area :: Shape -> Double
area (Triangle p1 p2 p3) =
    let
        a = len $ p1 - p2
        b = len $ p2 - p3
        c = len $ p3 - p1
        p = (a + b + c) / 2
     in
        sqrt $ p * (p - a) * (p - b) * (p - c)
area (Circle _ _ r) = pi * r ** 2

contains :: Shape -> Point -> Bool
contains (Triangle a b c) p =
    let
        t1 = Triangle a b p
        t2 = Triangle a c p
        t3 = Triangle b c p
     in
        abs (area (Triangle a b c) - area t1 - area t2 - area t3) <= 1e-6
contains _ _ = False
