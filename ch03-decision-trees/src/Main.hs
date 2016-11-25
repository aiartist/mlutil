module Main (main) where

import           Ch03DecisionTrees.DecisionTree hiding (F, Leaf, Node)
import qualified Ch03DecisionTrees.DecisionTree as DT
import           Ch03DecisionTrees.Flowchart hiding (flowchart)
import qualified Ch03DecisionTrees.Flowchart as FL
import           DataFiles
import           Diagrams.Backend.SVG.CmdLine
import           MLUtil
import           MLUtil.Graphics

data MyFeature = F String
instance ArrowLabel MyFeature where
    arrowLabel (F s) = Just s

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
    let c = FL.flowchart (mkDecisionTree dataSet labels)
    renderFlowchartSVG "flowchart.svg" c

example :: Diagram
example =
    let tree = Node "label0"
            [ (Node "label1" [(Leaf "N0", F "F2"), (Leaf "N1", F "F3"), (Leaf "N2", F "F4")], F "F0")
            , (Node "label2" [(Node "label1" [(Leaf "N0", F "F2"), (Leaf "N1", F "F3"), (Leaf "N2", F "F4")], F "F0"), (Leaf "N4", F "F6"), (Leaf "N5", F "F7")], F "F1")
            ]
    in flowchart tree

main :: IO ()
main = renderFigures >> mainWith example
