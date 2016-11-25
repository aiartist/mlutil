module Ch03DecisionTrees.Flowchart (flowchart) where

import           Ch03DecisionTrees.DecisionTree
import           Data.List.Split
import qualified Data.Map as M
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Graphics.SVGFonts

type NodeName = [Int]

nullNodeName :: NodeName
nullNodeName = [0]

flowchart :: DecisionTree -> Diagram B
flowchart = flowchartHelper nullNodeName Nothing

flowchartHelper :: NodeName -> Maybe Feature -> DecisionTree -> Diagram B
flowchartHelper name mf (Leaf (C c)) = leafBox (mkBoxCaption mf c)
flowchartHelper name mf (Node (L l) m) =
    let childInfo = foldr
                        (\(i, (f, tree)) acc -> let childName = i : name in (childName, flowchartHelper childName (Just f) tree # named childName) : acc)
                        []
                        (zip [0..] (M.toList m))
        childNames = map fst childInfo
        nodeDiagram = nodeBox (mkBoxCaption mf l) # named name
        childDiagrams = map snd childInfo
        arrows = map (\childName -> connectOutside name childName) childNames
    in (center nodeDiagram === center (hcat childDiagrams)) # compose arrows

compose :: [a -> a] -> a -> a
compose = foldr (.) id

mkBoxCaption :: Maybe Feature -> String -> String
mkBoxCaption Nothing s = s
mkBoxCaption (Just f) s = "(" ++ show (unFeature f) ++ ")\n" ++ s

leafBox :: String -> Diagram B
leafBox s = frame 1 $ centredText s 1 <> roundedRect 10 5 3

nodeBox :: String -> Diagram B
nodeBox s = frame 1 $ centredText s 1 <> rect 10 5

centredText :: String -> Double -> Diagram B
centredText s = centredLines (splitOn "\n" s)

centredLines :: [String] -> Double -> Diagram B
centredLines ls n = vcat' (with & catMethod .~ Distrib & sep .~ n) (map (\l -> centerX (text' l n)) ls)

text' :: String -> Double -> Diagram B
text' s n = textSVG_ (textOpts n) s # fc black # lw none

textOpts :: Double -> TextOpts Double
textOpts n = TextOpts lin2 INSIDE_H KERN False 1 n
