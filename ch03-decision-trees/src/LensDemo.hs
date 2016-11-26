module LensDemo (runLensDemos) where

import           Ch03DecisionTrees.DecisionTree
import           Data.List.Split
import           MLUtil.Graphics
import           Paths_ch03_decision_trees
import qualified System.IO.Strict as IOS

newtype LensFeature = LF String deriving (Eq, Ord, Show)
instance BranchLabel LensFeature where blLabel (LF s) = s

newtype LensClass = LC String deriving (Eq, Ord, Show)
instance LeafLabel LensClass where llLabel (LC s) = s

newtype LensLabel = LL String deriving Show
instance NodeLabel LensLabel where nlLabel (LL s) = s

testRenderDecisionTree :: IO ()
testRenderDecisionTree = do
    path <- getDataFileName "lenses.txt"
    ls <- lines <$> IOS.readFile path
    let dataSet = map (\l -> let xs = splitOneOf ['\t'] l in (map LF (init xs), (LC $ last xs))) ls
        labels = LL <$> ["age", "prescript", "astigmatic", "tearRate"]
        tree = mkDecisionTree dataSet labels
    renderFlowchartSVG "lens.svg" (flowchart tree)

runLensDemos :: IO ()
runLensDemos = testRenderDecisionTree
