import Lib (generateRandomPoint, generateRandomVector, generateRandomVectorCosDistribution)
import Point (Point (z), dotp, len, mul, normalize)
import Shape (Shape (..), area, contains)
import System.Random (randomRIO)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ QC.testProperty "Contains Test" prop_pointInside
        , QC.testProperty "Check Uniform Distibution for Triangle" prop_uniformDistributionTriangle
        , QC.testProperty "Check Uniform Distibution for Circle" prop_uniformDistributionCircle
        , QC.testProperty "Check Vector is on Sphere" prop_checkVectorIsOnSphere
        , QC.testProperty "Check Uniform Distibution for Sphere" prop_uniformDistributionSphere
        , QC.testProperty "Check Cosine Distibution for Sphere" prop_cosineDistributionSphere
        ]

prop_pointInside :: Shape -> Property
prop_pointInside shape = monadicIO $ do
    p <- run $ generateRandomPoint shape
    assert $ contains shape p

prop_uniformDistributionTriangle :: Point -> Point -> Point -> Property
prop_uniformDistributionTriangle p1 p2 p3 = monadicIO $ do
    let t = Triangle p1 p2 p3
    points <- run $ mapM (\_ -> generateRandomPoint t) [1 :: Int .. 100000]
    let expectedRatio = 100000 / area t
    assert $
        all
            ( \x ->
                let
                    a = (+) p3 $ flip mul x $ p1 - p3
                    b = (+) p3 $ flip mul x $ p2 - p3
                    t' = Triangle a b p3
                    numInArea = fromIntegral . length . filter (contains t') $ points
                 in
                    (>=) (0.1 * expectedRatio) $ abs $ expectedRatio - numInArea / area t'
            )
            [0.1, 0.2 .. 1]

prop_uniformDistributionCircle :: Point -> Point -> Property
prop_uniformDistributionCircle n p0 = monadicIO $ do
    r <- run $ randomRIO (0.1, 100)
    let c = Circle n p0 r
    points <- run $ mapM (\_ -> generateRandomPoint c) [1 :: Int .. 100000]
    let expectedRatio = 100000 / area c
    assert $
        all
            ( \x ->
                let
                    c' = Circle n p0 $ x * r
                    numInArea = fromIntegral . length . filter (contains c') $ points
                 in
                    (>=) (0.15 * expectedRatio) $ abs $ expectedRatio - numInArea / area c'
            )
            [0.1, 0.2 .. 1]

prop_checkVectorIsOnSphere :: Property
prop_checkVectorIsOnSphere = monadicIO $ do
    p <- run generateRandomVector
    assert $ (>=) 1e-6 $ abs $ len p - 1

prop_uniformDistributionSphere :: Property
prop_uniformDistributionSphere = monadicIO $ do
    vecs <- run $ mapM (const generateRandomVector) [1 :: Int .. 100000]
    let expectedRatio = (/) 100000 $ 4 * pi :: Double
    assert $
        all
            ( \h ->
                let
                    numOnStripe = fromIntegral . length $ filter (\v -> z v <= h && z v > h - 0.1) vecs
                 in
                    (>=) (0.1 * expectedRatio) $ abs $ expectedRatio - numOnStripe / (2 * pi * 0.1)
            )
            [0.1, 0.2 .. 1]

prop_cosineDistributionSphere :: Point -> Property
prop_cosineDistributionSphere h = monadicIO $ do
    let n = normalize h
    vecs <- run $ mapM (const $ generateRandomVectorCosDistribution n) [1 :: Int .. 100000]
    let coses = map (dotp n) vecs
    assert $
        all
            ( \k ->
                let
                    numOnStripe = fromIntegral . length $ filter (\c -> c <= k && c > k - 0.1) coses
                 in
                    (>=) 1e-2 $ abs $ (-) (k ** 2 - (k - 0.1) ** 2) $ numOnStripe / 100000
            )
            [0.1, 0.2 .. 1]
