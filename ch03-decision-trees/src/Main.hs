module Main (main) where

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

testClassify :: IO ()
testClassify = do
    let tree = mkDecisionTree dataSet labels
        r = classify tree labels [1, 0]
    print tree
    print r

main :: IO ()
main = do
    --renderFigures
    testClassify
