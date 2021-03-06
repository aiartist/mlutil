{-# LANGUAGE RecordWildCards #-}

module MLUtil.Graphics.Flowchart
    ( BranchLabel (..)
    , Flowchart (..)
    , LeafLabel (..)
    , NodeLabel (..)
    , flowchart
    ) where

import           MLUtil.Graphics.Imports
import           MLUtil.Tree

type Flowchart = Diagram
type FlowchartWithSize = (Flowchart, Size)
type Size = (Measure, Measure)

-- |A label for a branch  within a tree
class BranchLabel b where
    blLabel :: b -> String

-- |A label for a leaf within a tree
class LeafLabel l where
    llLabel :: l -> String

-- |A label for a node within a tree
class NodeLabel n where
    nlLabel :: n -> String

data FlowchartLayout = FlowchartLayout
    { flBoxInnerWidth :: Measure
    , flBoxInnerHeight :: Measure
    , flBoxFrameWidth :: Measure
    , flBranchFontSize :: FontMeasure
    , flLeafFontSize :: FontMeasure
    , flNodeFontSize :: FontMeasure
    }

defaultFlowchartLayout :: FlowchartLayout
defaultFlowchartLayout = FlowchartLayout
    { flBoxInnerWidth = 10
    , flBoxInnerHeight = 5
    , flBoxFrameWidth = 2
    , flBranchFontSize = 6
    , flLeafFontSize = 7
    , flNodeFontSize = 7
    }

flowchart :: (BranchLabel b, LeafLabel l, NodeLabel n) => Tree b l n -> Flowchart
flowchart = fst . (flowchartHelper defaultFlowchartLayout)

flowchartHelper :: (BranchLabel b, LeafLabel l, NodeLabel n) => FlowchartLayout -> Tree b l n -> FlowchartWithSize
flowchartHelper layout (Leaf s) = (leafBox layout (llLabel s), (boxOuterWidth layout, boxOuterHeight layout))
flowchartHelper layout@FlowchartLayout{..} (Node nodeLabel childArrows) =
    let
        boxOuterWidth' = boxOuterWidth layout
        boxOuterHeight' = boxOuterHeight layout
        h = length childArrows `quot` 2
        arrowStart = p2 (0, -(flBoxInnerHeight / 2))
        childDiagramInfos = map (\(A t al) -> let (d, (w, h)) = flowchartHelper layout t in (al, d, w, h)) childArrows

        -- TODO: Consider collapsing these into a single fold
        width = foldr (\(_, _, w, _) acc -> acc + w) 0 childDiagramInfos
        maxHeight = foldr (\(_, _, _, h) acc -> if h > acc then h else acc) 0 childDiagramInfos
        (childDiagrams, _) = foldr
            (\(_, d, w, _) (ds, x) -> let d' = d # moveTo (p2 (x - w / 2, -boxOuterHeight')) in (d' : ds, x - w))
            ([], width / 2)
            childDiagramInfos
        (branches, _) = foldr
            (\(_, d, w, _) (ds, x) -> let d = arrowBetween' (with & headLength .~ verySmall) arrowStart (p2 (x - w / 2, flBoxInnerHeight / 2 - boxOuterHeight')) in (d : ds, x - w))
            ([], width / 2)
            childDiagramInfos
        (branchLabels, _) = foldr
            (\(pos, (bl, _, w, _)) (ds, x) ->
                let d = text (blLabel bl) # fontSize flBranchFontSize # moveTo (p2 (x - w / 2, -(boxOuterHeight' / 2)))
                in (d : ds, x - w))
            ([], width / 2)
            (zip [-h ..] childDiagramInfos)

        height = maxHeight + boxOuterHeight'
    in
        (mconcat $
            nodeBox layout (nlLabel nodeLabel) # moveTo (p2 (0, 0)) :
            childDiagrams ++
            branches ++
            branchLabels
            , (width, height))

boxOuterWidth :: FlowchartLayout -> Measure
boxOuterWidth FlowchartLayout{..} = flBoxFrameWidth + flBoxInnerWidth + flBoxFrameWidth

boxOuterHeight :: FlowchartLayout -> Measure
boxOuterHeight FlowchartLayout{..} = flBoxFrameWidth + flBoxInnerHeight + flBoxFrameWidth

leafBox :: FlowchartLayout -> String -> Diagram
leafBox FlowchartLayout{..} s = frame flBoxFrameWidth $
    text s # fontSize flLeafFontSize <>
    roundedRect flBoxInnerWidth flBoxInnerHeight 3

nodeBox :: FlowchartLayout -> String -> Diagram
nodeBox FlowchartLayout{..} s = frame flBoxFrameWidth $
    text s # fontSize flNodeFontSize <>
    rect flBoxInnerWidth flBoxInnerHeight
