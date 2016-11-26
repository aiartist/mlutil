{-# LANGUAGE RecordWildCards #-}

module DecisionTreeSpec
    ( main
    , spec
    ) where

import           Ch03DecisionTrees.DecisionTree
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map as M
import           MLUtil
import           MLUtil.Graphics
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

leaf :: String -> DecisionTree
leaf = Leaf . C

node :: String -> [Arrow Feature] -> DecisionTree
node = Node . L

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
                    node "no surfacing"
                        [ A (leaf "no") (F 0)
                        , A (node "flippers"
                            [ A (leaf "no") (F 0)
                            , A (leaf "yes") (F 1)
                            ]) (F 1)
                        ]
            in mkDecisionTree dataSet labels `shouldBe` expectedDT

    describe "classify" $ do
        let tree =
                node "no surfacing"
                    [ A (leaf "no") (F 0)
                    , A (node "flippers"
                        [ A (leaf "no") (F 0)
                        , A (leaf "yes") (F 1)
                        ]) (F 1)
                    ]
            labels = L <$> ["no surfacing", "flippers"]

        it "should classify [0, 0] as no" $
            classify tree labels (F <$> [0, 0]) `shouldBe` C "no"

        it "should classify [0, 1] as no" $
            classify tree labels (F <$> [0, 1]) `shouldBe` C "no"

        it "should classify [1, 0] as no" $
            classify tree labels (F <$> [1, 0]) `shouldBe` C "no"

        it "should classify [1, 1] as yes" $
            classify tree labels (F <$> [1, 1]) `shouldBe` C "yes"

    {-
    describe "encodeDecisionTree" $ do
        let tree = mkDecisionTree dataSet labels
            bs = encode tree
            tree' = decode bs

        it "single leaf should roundtrip" $
            tree' `shouldBe` tree
    -}

main :: IO ()
main = hspec spec
