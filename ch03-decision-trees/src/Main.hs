module Main (main) where

import           Data.Binary
import           Ch03DecisionTrees.DecisionTree
import           MLUtil
import           MLUtil.Graphics

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
        r = classify tree labels [1, 0]
    print r
    encodeFile "test.bin" tree

data LensFeature = LF String deriving Show
instance FeatureClass LensFeature where

data LensClass = LC String deriving Show
instance ClassClass LensClass where

data LensLabel = LL String
instance LabelClass LensLabel where

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
