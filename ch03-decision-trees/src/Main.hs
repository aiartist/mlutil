module Main (main) where

import           Data.Binary
import           Data.List.Split
import           DataFiles
import           Ch03DecisionTrees.DecisionTree
import           MLUtil
import           MLUtil.Graphics
import qualified System.IO.Strict as IOS

newtype FishFeature = FF Int deriving (Eq, Ord)
instance BranchLabel FishFeature where blLabel (FF x) = show x

newtype FishClass = FC String deriving (Eq, Ord, Show)
instance LeafLabel FishClass where llLabel (FC x) = x

newtype FishLabel = FL String deriving Eq
instance NodeLabel FishLabel where nlLabel (FL x) = x

newtype LensFeature = LF String deriving (Eq, Ord, Show)

newtype LensClass = LC String deriving (Eq, Ord, Show)

newtype LensLabel = LL String deriving Show

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
renderFigures = do
    -- Figures 3.2 and 3.6
    let c = flowchart (mkDecisionTree fishDataSet fishLabels)
    renderFlowchartSVG "flowchart.svg" c

testClassifyAndEncode :: IO ()
testClassifyAndEncode = do
    let tree = mkDecisionTree fishDataSet fishLabels
        r = classify tree fishLabels (FF <$> [1, 0])
    print r
    --encodeFile "test.bin" tree

testLenses :: IO ()
testLenses = do
    path <- getDataFileName "lenses.txt"
    ls <- lines <$> IOS.readFile path
    let lenses = map (\l -> let xs = splitOneOf ['\t'] l in (map LF (init xs), (LC $ last xs))) ls
        lensesLabels = LL <$> ["age", "prescript", "astigmatic", "tearRate"]
        lensesTree = mkDecisionTree lenses lensesLabels
    print lenses
    print lensesTree

main :: IO ()
main = do
    --renderFigures
    --testClassifyAndEncode
    testLenses
