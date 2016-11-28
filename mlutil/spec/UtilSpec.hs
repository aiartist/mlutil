module UtilSpec
    ( main
    , spec
    ) where

import           MLUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "deleteAt" $ do
        it "fails on (-2)th element" $
            deleteAt (-2) [10, 20, 30, 40] `shouldBe` Nothing
        it "fails on (-1)th element" $
            deleteAt (-1) [10, 20, 30, 40] `shouldBe` Nothing
        it "removes 0th element" $
            deleteAt 0 [10, 20, 30, 40] `shouldBe` Just [20, 30, 40]
        it "removes 1th element" $
            deleteAt 1 [10, 20, 30, 40] `shouldBe` Just [10, 30, 40]
        it "removes 2th element" $
            deleteAt 2 [10, 20, 30, 40] `shouldBe` Just [10, 20, 40]
        it "removes (n - 1)th element" $
            deleteAt 3 [10, 20, 30, 40] `shouldBe` Just [10, 20, 30]
        it "fails on nth element" $
            deleteAt 4 [10, 20, 30, 40] `shouldBe` Nothing
        it "fails on (n + 1)th element" $
            deleteAt 5 [10, 20, 30, 40] `shouldBe` Nothing

    describe "removeAt" $ do
        it "fails on (-2)th element" $
            removeAt (-2) [10, 20, 30, 40] `shouldBe` Nothing
        it "fails on (-1)th element" $
            removeAt (-1) [10, 20, 30, 40] `shouldBe` Nothing
        it "removes and returns 0th element" $
            removeAt 0 [10, 20, 30, 40] `shouldBe` Just ([20, 30, 40], 10)
        it "removes and returns 1th element" $
            removeAt 1 [10, 20, 30, 40] `shouldBe` Just ([10, 30, 40], 20)
        it "removes and returns 2th element" $
            removeAt 2 [10, 20, 30, 40] `shouldBe` Just ([10, 20, 40], 30)
        it "removes and returns (n - 1)th element" $
            removeAt 3 [10, 20, 30, 40] `shouldBe` Just ([10, 20, 30], 40)
        it "fails on nth element" $
            removeAt 4 [10, 20, 30, 40] `shouldBe` Nothing
        it "fails on (n + 1)th element" $
            removeAt 5 [10, 20, 30, 40] `shouldBe` Nothing

main :: IO ()
main = hspec spec
