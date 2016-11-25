{-# LANGUAGE RecordWildCards #-}

module DecisionTreeSpec
    ( main
    , spec
    ) where

import           Ch03DecisionTrees.DecisionTree
import qualified Data.Map as M
import           MLUtil.Test
import           Test.Hspec

dataSet :: [Record]
dataSet =
    [ (F <$> [1, 1], C "yes")
    , (F <$> [1, 1], C "yes")
    , (F <$> [1, 0], C "no")
    , (F <$> [0, 1], C "no")
    , (F <$> [0, 1], C "no")
    ]

labels :: [Label]
labels = L <$> ["no surfacing", "flippers"]

spec :: Spec
spec = do
    describe "calculateShannonEntropy" $ do
        it "should return correct value" $
            calculateShannonEntropy dataSet `shouldRoundTo` 0.97095

    describe "splitDataSet" $ do
        it "should split 0, 0" $
            splitDataSet dataSet 0 (F 0) `shouldBe` [(F <$> [1], C "no"), (F <$> [1], C "no")]

        it "should split 0, 1" $
            splitDataSet dataSet 0 (F 1) `shouldBe` [(F <$> [1], C "yes"), (F <$> [1], C "yes"), (F <$> [0], C "no")]

        it "should split 1, 0" $
            splitDataSet dataSet 1 (F 0) `shouldBe` [(F <$> [1], C "no")]

        it "should split 1, 1" $
            splitDataSet dataSet 1 (F 1) `shouldBe` [(F <$> [1], C "yes"), (F <$> [1], C "yes"), (F <$> [0], C "no"), (F <$> [0], C "no")]

    describe "chooseBestFeatureToSplit" $ do
        it "calculate correctly" $ do
            let (gain, idx) = chooseBestFeatureToSplit dataSet
            gain `shouldRoundTo` 0.41997
            idx `shouldBe` 0

    describe "majorityCount" $ do
        it "should get majority count" $
            majorityCount (map snd dataSet) `shouldBe` (C "no", 3)

    describe "mkDecisionTree" $ do
        it "should create a decision tree" $
            let expectedDT =
                    Node
                        (L "no surfacing")
                        (M.fromList [(F 0, Leaf $ C "no"), (F 1,
                        Node
                            (L "flippers")
                            (M.fromList [(F 0, Leaf $ C "no"), (F 1, Leaf $ C "yes")]))])
            in mkDecisionTree dataSet labels `shouldBe` expectedDT

main :: IO ()
main = hspec spec
