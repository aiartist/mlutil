{-# LANGUAGE RecordWildCards #-}

module Ch04NaiveBayes.NaiveBayes
    ( NaiveBayesModel (..)
    , WordVector
    , trainNB0
    , classifyNB
    ) where

import qualified Data.Vector as V
import           MLUtil.Primitives

type WordVector = V.Vector Int

data NaiveBayesModel = NaiveBayesModel
    { nbmP0Vector :: V.Vector Double
    , nbmP1Vector :: V.Vector Double
    , nbmPClass1 :: Double
    } deriving Show

-- cf bayes.trainNB0
trainNB0 :: [(WordVector, Int)] -> NaiveBayesModel
trainNB0 rows =
    let columnCount = (V.length . fst . head) rows
        initialNum = V.replicate columnCount 1
        initialDenom = V.replicate columnCount 2
        (rowCount, p0Num, p0Denom, p1Num, p1Denom, classTotal) = foldr (\(xs, c) (n, p0Num, p0Denom, p1Num, p1Denom, classTotal) ->
            if c == 1
                then (n + 1, p0Num, p0Denom, addV p1Num xs, addS (sumV xs) p1Denom, classTotal + c)
                else (n + 1, addV p0Num xs, addS (sumV xs) p0Denom, p1Num, p1Denom, classTotal + c)
            )
            (0, initialNum, initialDenom, initialNum, initialDenom, 0)
            rows
        p0Vector = logV $ divV p0Num p0Denom
        p1Vector = logV $ divV p1Num p1Denom
        pAbusive = fromIntegral classTotal / fromIntegral rowCount
    in NaiveBayesModel p0Vector p1Vector pAbusive

-- cf bayes.classifyNB
classifyNB :: NaiveBayesModel -> WordVector -> Int
classifyNB NaiveBayesModel{..} xs =
    let xsF = floatV xs
        p0 = sumV (mulV xsF nbmP0Vector) + log (1.0 - nbmPClass1)
        p1 = sumV (mulV xsF nbmP1Vector) + log nbmPClass1
    in if p1 > p0 then 1 else 0
