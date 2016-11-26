module Main (main) where

import           Data.Binary
import           Data.List.Split
import           DataFiles
import           Ch03DecisionTrees.DecisionTree
import           MLUtil
import           MLUtil.Graphics
import qualified System.IO.Strict as IOS

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

renderFigures :: IO ()
renderFigures = do
    -- Figures 3.2 and 3.6
    let c = flowchart (mkDecisionTree dataSet labels)
    renderFlowchartSVG "flowchart.svg" c

testClassifyAndEncode :: IO ()
testClassifyAndEncode = do
    let tree = mkDecisionTree dataSet labels
        r = classify tree labels (F <$> [1, 0])
    print r
    --encodeFile "test.bin" tree

class FeatureClass2 a where
class ClassClass2 a where
class LabelClass2 a where

data LensFeature = LF String deriving Show
instance FeatureClass2 LensFeature where

data LensClass = LC String deriving Show
instance ClassClass2 LensClass where

data LensLabel = LL String
instance LabelClass2 LensLabel where

-- TODO: This is roughly what Record should look like eventually
data Record2 a b = R2 [a] b deriving Show

mkDecisionTree2 :: [Record2 a b] -> [c] -> Int
mkDecisionTree2 _ _ = 0

lenses :: IO ()
lenses = do
    path <- getDataFileName "lenses.txt"
    ls <- lines <$> IOS.readFile path
    let lenses = map (\l -> let xs = splitOneOf ['\t'] l in R2 (map LF (init xs)) (LC $ last xs)) ls
        lensesLabels = LL <$> ["age", "prescript", "astigmatic", "tearRate"]
        lensesTree = mkDecisionTree2 lenses lensesLabels
    print lenses
    print lensesTree

main :: IO ()
main = do
    --renderFigures
    --testClassifyAndEncode
    lenses
