module FishDemo (runFishDemos) where

import           Ch03DecisionTrees.DecisionTree
import           Data.Binary
import           MLUtil.Graphics

newtype FishFeature = FF Int deriving (Eq, Ord)
instance BranchLabel FishFeature where blLabel (FF x) = show x

newtype FishClass = FC String deriving (Eq, Ord, Show)
instance LeafLabel FishClass where llLabel (FC x) = x

newtype FishLabel = FL String deriving Eq
instance NodeLabel FishLabel where nlLabel (FL x) = x

type FishRecord = Record FishFeature FishClass

mkFishRecord :: ([Int], String) -> FishRecord
mkFishRecord (fs, c) = (FF <$> fs, FC c)

fishDataSet :: [FishRecord]
fishDataSet = mkFishRecord <$>
    [ ([1, 1], "yes")
    , ([1, 1], "yes")
    , ([1, 0], "no")
    , ([0, 1], "no")
    , ([0, 1], "no")
    ]

fishLabels :: [FishLabel]
fishLabels = FL <$> ["no surfacing", "flippers"]

renderFigures :: IO ()
renderFigures =
    -- Figures 3.2 and 3.6
    renderFlowchartSVG "flowchart.svg" (flowchart (mkDecisionTree fishDataSet fishLabels))

testClassify :: IO ()
testClassify = do
    let tree = mkDecisionTree fishDataSet fishLabels
        r = classify tree fishLabels (FF <$> [1, 0])
    print r

testEncode :: IO ()
testEncode = --encodeFile "test.bin" (mkDecisionTree fishDataSet fishLabels)
    return ()

runFishDemos :: IO ()
runFishDemos = do
    --renderFigures
    --testClassify
    --testEncode
    return ()
