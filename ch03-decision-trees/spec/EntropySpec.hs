{-# LANGUAGE RecordWildCards #-}

module EntropySpec
    ( main
    , spec
    ) where

import           Ch03DecisionTrees.Entropy
import qualified Data.Map as M
import           MLUtil.Test
import           Test.Hspec

dataSet :: [Record]
dataSet =
    [ ([1, 1], Class "yes")
    , ([1, 1], Class "yes")
    , ([1, 0], Class "no")
    , ([0, 1], Class "no")
    , ([0, 1], Class "no")
    ]

labels :: [Label]
labels = Label <$> ["no surfacing", "flippers"]

spec :: Spec
spec = do
    describe "calculateShannonEntropy" $ do
        it "should return correct value" $
            calculateShannonEntropy dataSet `shouldRoundTo` 0.97095

    describe "splitDataSet" $ do
        it "should split 0, 0" $
            splitDataSet dataSet 0 0 `shouldBe` [([1], Class "no"), ([1], Class "no")]

        it "should split 0, 1" $
            splitDataSet dataSet 0 1 `shouldBe` [([1], Class "yes"), ([1], Class "yes"), ([0], Class "no")]

        it "should split 1, 0" $
            splitDataSet dataSet 1 0 `shouldBe` [([1], Class "no")]

        it "should split 1, 1" $
            splitDataSet dataSet 1 1 `shouldBe` [([1], Class "yes"), ([1], Class "yes"), ([0], Class "no"), ([0], Class "no")]

    describe "chooseBestFeatureToSplit" $ do
        it "calculate correctly" $ do
            let (gain, idx) = chooseBestFeatureToSplit dataSet
            gain `shouldRoundTo` 0.41997
            idx `shouldBe` 0

    describe "majorityCount" $ do
        it "should get majority count" $
            majorityCount (map snd dataSet) `shouldBe` (Class "no", 3)

    describe "mkDecisionTree" $ do
        it "should create a decision tree" $
            let expectedDT =
                    DT1
                        (Label "no surfacing")
                        (M.fromList [(0, DT0 $ Class "no"), (1,
                        DT1
                            (Label "flippers")
                            (M.fromList [(0, DT0 $ Class "no"), (1, DT0 $ Class "yes")]))])
            in mkDecisionTree dataSet labels `shouldBe` expectedDT

main :: IO ()
main = hspec spec
