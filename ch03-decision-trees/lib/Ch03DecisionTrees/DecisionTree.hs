module Ch03DecisionTrees.DecisionTree
    ( Record (..)
    , calculateShannonEntropy
    , chooseBestFeatureToSplit
    , classify
    , majorityCount
    , mkDecisionTree
    , splitDataSet
    ) where

-- TODO: Hide the (!!) operator so we don't index into lists!

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import           MLUtil
import           MLUtil.Graphics

type Record a l = ([a], l)

-- cf trees.majorityCnt
majorityCount :: Ord a => [a] -> (a, Int)
majorityCount labels = foldr1
    (\p0@(_, n0) p1@(_, n1) -> if n1 > n0 then p1 else p0)
    (M.toList $ itemCounts labels)

-- cf trees.calcShannonEnt
calculateShannonEntropy :: Ord l => [Record a l] -> Double
calculateShannonEntropy rs =
    let count = length rs
    in foldr
        (\n entropy -> let prob = probability n count in entropy -  prob * log2 prob)
        0.0
        (itemCounts (map snd rs))
    where probability n count = fromIntegral n / fromIntegral count
          log2 x = logBase 2 x

-- cf trees.splitDataSet
-- TODO: Use vector instead of list for O(1) indexing
splitDataSet :: Eq a => [Record a l] -> Int -> a -> [Record a l]
splitDataSet rs axis value = map
    (\(xs, l) -> let Just xs' = deleteAt axis xs in (xs', l)) $ filter (\(xs, _) -> xs !! axis == value)
    rs

-- cf trees.chooseBestFeatureToSplit
-- TODO: Use vector instead of list for O(1) indexing
chooseBestFeatureToSplit :: (Ord a, Ord l) => [Record a l] -> (Double, Int)
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

-- cf trees.createTree
-- TODO: Enforce non-emptiness
mkDecisionTree :: (Ord a, Eq l, Ord l) => [Record a l] -> [n] -> Tree a l n
mkDecisionTree dataSet labels =
    let classList = map snd dataSet
        firstClass = head classList
    in if (all (firstClass ==) classList)
        then Leaf firstClass -- Stop splitting when all of the classes are equal
        else if (0 == (length . fst . head) dataSet)
            then Leaf $ fst (majorityCount classList) -- Stop splitting when there are no more features in data set
            else
                let (_, bestFeat) = chooseBestFeatureToSplit dataSet
                    bestFeatLabel = labels !! bestFeat
                    Just labels' = deleteAt bestFeat labels
                    featValues = [features !! bestFeat | (features, _) <- dataSet]
                    uniqueVals = S.fromList featValues
                    childArrows = foldr (\value cts ->
                        let sp = splitDataSet dataSet bestFeat value
                            subtree = mkDecisionTree sp labels'
                        in (A subtree value) : cts)
                        []
                        uniqueVals
                in Node bestFeatLabel childArrows

-- cf trees.classify
-- TODO: Use vector of keys instead of list for O(1) lookup
classify :: (Eq a, Eq n) => Tree a l n -> [n] -> [a] -> l
classify (Leaf l) _ _ = l
classify (Node nodeLabel arrows) featLabels testVec =
    let Just featIndex = nodeLabel `L.elemIndex` featLabels
        key = testVec !! featIndex
        Just (A subtree _) = L.find (\(A _ s) -> s == key) arrows
    in classify subtree featLabels testVec
