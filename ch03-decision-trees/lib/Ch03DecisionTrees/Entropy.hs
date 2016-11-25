{-# LANGUAGE LambdaCase #-}

module Ch03DecisionTrees.Entropy
    ( Label
    , Record
    , calculateShannonEntropy
    , chooseBestFeatureToSplit
    , majorityCount
    , mkDecisionTree
    , splitDataSet
    ) where

-- TODO: Hide the (!!) operator so we can't index into lists!

import qualified Data.Map as M
import qualified Data.Set as S

type Label = String
type Record = ([Int], Label)

itemCounts :: Ord a => [a] -> M.Map a Int
itemCounts = foldr
    (M.alter (\case { Nothing -> Just 1; Just n -> Just $ n + 1 }))
    M.empty

-- cf trees.calcShannonEnt
calculateShannonEntropy :: [Record] -> Double
calculateShannonEntropy rs =
    let count = length rs
    in foldr
        (\n entropy -> let prob = probability n count in entropy -  prob * log2 prob)
        0.0
        (itemCounts (map snd rs))
    where probability n count = fromIntegral n / fromIntegral count
          log2 x = logBase 2 x

-- cf trees.splitDataSet
-- TODO: Use vector instead of list for O(N) indexing
splitDataSet :: [Record] -> Int -> Int -> [Record]
splitDataSet rs axis value =
    map (\(xs, l) -> (deleteAt axis xs, l)) $ filter (\(xs, _) -> xs !! axis == value) rs
    where deleteAt idx xs =
            let (b, e) = splitAt idx xs
            in b ++ drop 1 e

-- cf trees.chooseBestFeatureToSplit
-- TODO: Use vector instead of list for O(N) indexing
chooseBestFeatureToSplit :: [Record] -> (Double, Int)
chooseBestFeatureToSplit rs =
    let (xs, _) = head rs
        featureCount = length xs
        baseEntropy = calculateShannonEntropy rs
    in foldr
        (\i p@(bestGain, bestIdx) ->
            let s = S.fromList [let (xs, _) = example in xs !! i | example <- rs]
                newEntropy = foldr (\a e ->
                    let rs' = splitDataSet rs i a
                        prob = fromIntegral (length rs') / fromIntegral (length rs)
                    in e + prob * calculateShannonEntropy rs')
                    0.0
                    s
                gain = baseEntropy - newEntropy
            in if gain > bestGain then (gain, i) else (bestGain, bestIdx))
        (0.0, -1)
        [0..featureCount - 1]

-- cf trees.majorityCnt
majorityCount :: [Label] -> (Label, Int)
majorityCount labels = foldr1
    (\p0@(_, n0) p1@(_, n1) -> if n1 > n0 then p1 else p0)
    (M.toList $ itemCounts labels)

-- cf trees.createTree
mkDecisionTree :: [Record] -> Int
mkDecisionTree = length
