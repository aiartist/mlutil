{-# LANGUAGE RecordWildCards #-}

module Ch04NaiveBayes.NaiveBayes
    ( Classification (..)
    , NaiveBayesModel (..)
    , trainNB0
    , classifyNB
    ) where

import qualified Data.Vector as V
import           MLUtil.Primitives

data Classification = Class0 | Class1 deriving (Eq, Show)

data NaiveBayesModel = NaiveBayesModel
    { nbmP0Vector :: V.Vector Double
    , nbmP1Vector :: V.Vector Double
    , nbmPClass1 :: Double
    } deriving Show

-- TODO: Not sure whether the cost of converting from Classification to Int
-- is worth it just to get type safety...
classificationToInt :: Classification -> Int
classificationToInt Class0 = 0
classificationToInt Class1 = 1

-- cf bayes.trainNB0
trainNB0 :: [(V.Vector Int, Classification)] -> NaiveBayesModel
trainNB0 rows =
    let columnCount = (V.length . fst . head) rows
        initialNum = V.replicate columnCount 1
        initialDenom = V.replicate columnCount 2
        (rowCount, p0Num, p0Denom, p1Num, p1Denom, classTotal) = foldr (\(xs, c) (n, p0Num, p0Denom, p1Num, p1Denom, classTotal) ->
            let c' = classificationToInt c
            in if c' == 1
                then (n + 1, p0Num, p0Denom, addV p1Num xs, addS (sumV xs) p1Denom, classTotal + c')
                else (n + 1, addV p0Num xs, addS (sumV xs) p0Denom, p1Num, p1Denom, classTotal + c')
            )
            (0, initialNum, initialDenom, initialNum, initialDenom, 0)
            rows
        p0Vector = logV $ divV p0Num p0Denom
        p1Vector = logV $ divV p1Num p1Denom
        pAbusive = fromIntegral classTotal / fromIntegral rowCount
    in NaiveBayesModel p0Vector p1Vector pAbusive

-- cf bayes.classifyNB
classifyNB :: NaiveBayesModel -> V.Vector Int -> Classification
classifyNB NaiveBayesModel{..} xs =
    let xsF = floatV xs
        p0 = sumV (mulV xsF nbmP0Vector) + log (1.0 - nbmPClass1)
        p1 = sumV (mulV xsF nbmP1Vector) + log nbmPClass1
    in if p1 > p0 then Class1 else Class0
