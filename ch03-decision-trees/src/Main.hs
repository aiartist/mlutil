module Main (main) where

import           Data.Binary
import           Data.List.Split
import           DataFiles
import           Ch03DecisionTrees.DecisionTree
import           MLUtil
import           MLUtil.Graphics
import qualified System.IO.Strict as IOS

-- Arrow: Eq a, Ord a
newtype Feature = F { unFeature :: Int } deriving (Eq, Ord)
instance ArrowLabel Feature where alLabel = show . unFeature

-- Leaf: Eq l, Ord l
newtype Class = C { unClass :: String } deriving (Eq, Ord, Show)
instance LeafLabel Class where llLabel = unClass

-- Node: Eq n
newtype Label = L { unLabel :: String } deriving Eq
instance NodeLabel Label where nlLabel = unLabel

dataSet :: [Record Feature Class]
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

newtype LensFeature = LF String deriving (Eq, Ord, Show)
newtype LensClass = LC String deriving (Eq, Ord, Show)
newtype LensLabel = LL String deriving Show

lenses :: IO ()
lenses = do
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
    lenses
