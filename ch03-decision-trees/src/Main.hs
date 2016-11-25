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
    let e = calculateShannonEntropy dataSet
        sp = splitDataSet dataSet 0 (F 1)
        result = chooseBestFeatureToSplit dataSet
    print result
    putStrLn "Done"
