module Main (main) where

import           Ch03DecisionTrees.DecisionTree hiding (F, Leaf, Node)
import qualified Ch03DecisionTrees.DecisionTree as DT
import           DataFiles
import           Diagrams.Backend.SVG.CmdLine
import           MLUtil
import           MLUtil.Graphics

dataSet :: [Record]
dataSet =
    [ (DT.F <$> [1, 1], C "yes")
    , (DT.F <$> [1, 1], C "yes")
    , (DT.F <$> [1, 0], C "no")
    , (DT.F <$> [0, 1], C "no")
    , (DT.F <$> [0, 1], C "no")
    ]

labels :: [Label]
labels = L <$> ["no surfacing", "flippers"]

renderFigures :: IO ()
renderFigures = do
    -- Figure 3.2
    let c = flowchart (mkDecisionTree dataSet labels)
    renderFlowchartSVG "flowchart.svg" c

data StringFeature = F String
instance ArrowLabel StringFeature where
    arrowLabel (F s) = Just s

node = Node . L
leaf = Leaf . C

example :: Diagram
example =
    let tree = node "label0"
            [ (node "label1" [(leaf "N0", F "F2"), (leaf "N1", F "F3"), (leaf "N2", F "F4")], F "F0")
            , (node "label2" [(node "label1" [(leaf "N0", F "F2"), (leaf "N1", F "F3"), (leaf "N2", F "F4")], F "F0"), (leaf "N4", F "F6"), (leaf "N5", F "F7")], F "F1")
            ]
    in flowchart tree

main :: IO ()
main = renderFigures >> mainWith example
