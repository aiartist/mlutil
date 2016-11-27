module VectorPrimSpec
    ( main
    , spec
    ) where

import           MLUtil.Primitives
import           MLUtil.Test
import qualified Data.Vector as V
import           Data.Vector ((!))
import           Test.Hspec

spec :: Spec
spec = do
    describe "addS" $
        it "adds scalar to vector" $
            10 `addS` V.fromList [4, 5, 6] `shouldBe` V.fromList [14, 15, 16]

    describe "addV" $
        it "adds two vectors" $
            V.fromList [1, 2, 3] `addV` V.fromList [4, 5, 6] `shouldBe` V.fromList [5, 7, 9]

    describe "divV" $
        it "divides two vectors elementwise" $
            V.fromList [10, 20, 30] `divV` V.fromList [2, 4, 6] `shouldBe` V.fromList [5.0, 5.0, 5.0]

    describe "floatV" $
        it "converts elements of vector of Int to Double" $
            floatV (V.fromList [10, 20, 30]) `shouldBe` V.fromList [10.0, 20.0, 30.0]

    describe "logV" $
        it "applies log to elements of vector" $ do
            let result :: V.Vector Double
                result = logV (V.fromList [1000.0, 100.0, 10.0])
            result ! 0 `shouldRoundTo` 6.90776
            result ! 1 `shouldRoundTo` 4.60517
            result ! 2 `shouldRoundTo` 2.30259

    describe "mulV" $
        it "multiplies elements of vectors" $
            V.fromList [1, 2, 3] `mulV` V.fromList [2, 3, 4] `shouldBe` V.fromList [2, 6, 12]

    describe "sumV" $
        it "returns sum of elements of vector" $
            sumV (V.fromList [1, 2, 3]) `shouldBe` 6

main :: IO ()
main = hspec spec
