{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import           Ch03DecisionTrees.Entropy
import           DataFiles

dataSet :: [Record]
dataSet =
    [ ([1, 1], Class "yes")
    , ([1, 1], Class "yes")
    , ([1, 0], Class "no")
    , ([0, 1], Class "no")
    , ([0, 1], Class "no")
    ]

labels :: [Label]
labels = Label <$> ["no surfacing", "flippers"]

main :: IO ()
main = do
    let e = calculateShannonEntropy dataSet
        sp = splitDataSet dataSet 0 1
        result = chooseBestFeatureToSplit dataSet
    print result
    putStrLn "Done"
