module RandomSpec
    ( main
    , spec
    ) where

import           MLUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "choiceExtract" $ do
        it "extracts random elements" $ do
            let xs = [10, 20, 30]

            Just (xs', x') <- choiceExtract xs
            length xs' `shouldBe` 2
            x' `shouldSatisfy` (`elem` xs)

            Just (xs'', x'') <- choiceExtract xs'
            length xs'' `shouldBe` 1
            x'' `shouldSatisfy` (\x -> x `elem` xs && x /= x')

            Just (xs''', x''') <- choiceExtract xs''
            length xs''' `shouldBe` 0
            x''' `shouldSatisfy` (\x -> x `elem` xs && x /= x' && x /= x'')

        it "fails on empty list" $ do
            r <- choiceExtract [] :: IO (Maybe ([Int], Int))
            r `shouldBe` Nothing

    describe "choiceExtractN" $ do
        it "returns original list and empty if n is 0 and list is empty" $ do
            Just (xs, ys) <- choiceExtractN 0 [] :: IO (Maybe ([Int], [Int]))
            xs `shouldBe` []
            ys `shouldBe` []

        it "returns original list and empty if n is 0 and list is non-empty" $ do
            Just (xs, ys) <- choiceExtractN 0 [10, 20, 30]
            xs `shouldBe` [10, 20, 30]
            ys `shouldBe` []

        it "fails for n > 0 if list is empty" $ do
            r <- choiceExtractN 1 [] :: IO (Maybe ([Int], [Int]))
            r `shouldBe` Nothing

        it "fails if n is greater than length of list" $ do
            r <- choiceExtractN 3 [10, 20]
            r `shouldBe` Nothing

        it "fails if n is negative" $ do
            r <- choiceExtractN (-1) [10, 20]
            r `shouldBe` Nothing

        it "extracts n random elements" $ do
            let xs = [10, 20, 30, 40]

            Just (xs', x') <- choiceExtractN 2 xs
            length xs' `shouldBe` 2
            x' !! 0 `shouldSatisfy` (`elem` xs)
            x' !! 1 `shouldSatisfy` (`elem` xs)
            x' !! 0 `shouldNotBe` x' !! 1

main :: IO ()
main = hspec spec
