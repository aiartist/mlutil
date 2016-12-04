module RandomSpec
    ( main
    , spec
    ) where

import           Control.Monad
import           Data.Maybe
import           MLUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "extract" $ do
        let Just ei = mkExtractIndices 10 [9, 8, 7, 6, 5]

        it "should fail if n are not consistent" $
            extract ei [0 .. 10] `shouldBe` Nothing

        it "should work" $ do
            let Just (residual, xs) = extract ei [0 .. 9]
            xs `shouldBe` [9, 8, 7, 6, 5]
            residual `shouldBe` [0, 1, 2, 3, 4]

    describe "choiceExtractIndices" $ do
        it "should fail for c < 0, n = 0" $
            isNothing (choiceExtractIndices (-1) 0) `shouldBe` True

        it "should fail for c = 0, n < 0" $
            isNothing (choiceExtractIndices 0 (-1)) `shouldBe` True

        it "should fail for c > n" $
            isNothing (choiceExtractIndices 10 9) `shouldBe` True

        let testCases =
                [ (0, 0)
                , (0, 10)
                , (100, 200)
                , (200, 200)
                ]
        forM_ testCases $ \(c, n) ->
            it ("should be valid for c = " ++ show c ++ ", n = " ++ show n) $ do
                let Just action = choiceExtractIndices c n
                ei <- action
                eiC ei `shouldBe` c
                eiN ei `shouldBe` n
                length (eiIs ei) `shouldBe` c
                ei `shouldSatisfy` isValidExtractIndices

    describe "choiceExtract" $ do
        it "returns original list and empty if n is 0 and list is empty" $ do
            Just (xs, ys) <- choiceExtract 0 [] :: IO (Maybe ([Int], [Int]))
            xs `shouldBe` []
            ys `shouldBe` []

        it "returns original list and empty if n is 0 and list is non-empty" $ do
            Just (xs, ys) <- choiceExtract 0 [10, 20, 30]
            xs `shouldBe` [10, 20, 30]
            ys `shouldBe` []

        it "fails for n > 0 if list is empty" $ do
            r <- choiceExtract 1 [] :: IO (Maybe ([Int], [Int]))
            r `shouldBe` Nothing

        it "fails if n is greater than length of list" $ do
            r <- choiceExtract 3 [10, 20]
            r `shouldBe` Nothing

        it "fails if n is negative" $ do
            r <- choiceExtract (-1) [10, 20]
            r `shouldBe` Nothing

        it "extracts n random elements" $ do
            let xs = [10, 20, 30, 40]

            Just (xs', x') <- choiceExtract 2 xs
            length xs' `shouldBe` 2
            x' !! 0 `shouldSatisfy` (`elem` xs)
            x' !! 1 `shouldSatisfy` (`elem` xs)
            x' !! 0 `shouldNotBe` x' !! 1

main :: IO ()
main = hspec spec
