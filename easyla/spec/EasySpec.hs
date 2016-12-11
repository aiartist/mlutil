module EasySpec
    ( main
    , spec
    ) where

import           Numeric.LinearAlgebra.Easy
import           Test.Hspec

spec :: Spec
spec = do
    describe "basics" $ do
        it "does the expected thing" $ do
            let m = (2 >< 3) [0..]
                x = 3 |> [0..]
                y = m #> x
                x' = asColumn x
                y' = m <> x'
                y'' = x <# (3 >< 2) [0..]
                a = ones 2 :: Vector
            print y
            print y'
            print y''
            print a
            print $ (ones (3, 2) :: Matrix)
            print $ (zeros (5, 7) :: Matrix)

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
