import Lib (generateRandomPoint)
import Shape (Shape (..), contains)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain triangleTests

triangleTests :: TestTree
triangleTests = testGroup "Contains Tests" [QC.testProperty "" prop_pointInside]

prop_pointInside :: Shape -> Property
prop_pointInside t = monadicIO $ do
    p <- run $ generateRandomPoint t
    assert $ contains t p
