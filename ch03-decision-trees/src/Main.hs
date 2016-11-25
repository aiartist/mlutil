{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import           Ch03DecisionTrees.DecisionTree
import           DataFiles

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

main :: IO ()
main = do
    let tree = mkDecisionTree dataSet labels
    print tree
    putStrLn "Done"
