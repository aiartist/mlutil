module STSpec
    ( main
    , spec
    ) where

import           Numeric.LinearAlgebra.Easy
import           Test.Hspec

spec :: Spec
spec = do
    describe "withSTMatrix" $ do
        it "should create expected matrix" $ do
            let m = withSTMatrix 9 3 3 $ \m -> do
                        writeMatrix m 0 0 10.0
                        writeMatrix m 1 1 20.0
                        writeMatrix m 2 2 30.0
            m `atIndex` (0, 0) `shouldBe` 10.0
            m `atIndex` (0, 1) `shouldBe` 9.0
            m `atIndex` (0, 2) `shouldBe` 9.0
            m `atIndex` (1, 0) `shouldBe` 9.0
            m `atIndex` (1, 1) `shouldBe` 20.0
            m `atIndex` (1, 2) `shouldBe` 9.0
            m `atIndex` (2, 0) `shouldBe` 9.0
            m `atIndex` (2, 1) `shouldBe` 9.0
            m `atIndex` (2, 2) `shouldBe` 30.0

main :: IO ()
main = hspec spec
