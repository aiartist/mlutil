module LensDemo (runLensDemos) where

import           Ch03DecisionTrees.DecisionTree
import           Data.List.Split
import           Paths_ch03_decision_trees
import qualified System.IO.Strict as IOS

newtype LensFeature = LF String deriving (Eq, Ord, Show)

newtype LensClass = LC String deriving (Eq, Ord, Show)

newtype LensLabel = LL String deriving Show

testMkDecisionTree :: IO ()
testMkDecisionTree = do
    path <- getDataFileName "lenses.txt"
    ls <- lines <$> IOS.readFile path
    let lenses = map (\l -> let xs = splitOneOf ['\t'] l in (map LF (init xs), (LC $ last xs))) ls
        lensesLabels = LL <$> ["age", "prescript", "astigmatic", "tearRate"]
        lensesTree = mkDecisionTree lenses lensesLabels
    print lenses
    print lensesTree

runLensDemos :: IO ()
runLensDemos = testMkDecisionTree
