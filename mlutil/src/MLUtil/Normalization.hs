{-# LANGUAGE RecordWildCards #-}

module MLUtil.Normalization
    ( ColumnNormalization (..)
    , MatrixNormalization (..)
    , normalizeColumn
    , normalizeMatrixColumns
    ) where

import qualified Data.Vector.Storable as VS
import           MLUtil.Folding
import           MLUtil.Imports

data ColumnNormalization = ColumnNormalization
    { cnValues :: Vector
    , cnRange :: R
    , cnMin :: R
    } deriving (Eq, Show)

data MatrixNormalization = MatrixNormalization
    { mnValues :: Matrix
    , mnRanges :: [R]
    , mnMins :: [R]
    } deriving Show

normalizeColumn :: Matrix -> Int -> ColumnNormalization
normalizeColumn m c =
    let initialMinMax = columnHead m c
        (xMin, xMax) = foldColumn (\x (xMin, xMax) -> (min x xMin, max x xMax)) (initialMinMax, initialMinMax) m c
        range = xMax - xMin
    in ColumnNormalization
        (VS.generate (rows m) (\r -> (m `atIndex` (r, c) - xMin) / range))
        range
        xMin

-- cf kNN.autoNorm
normalizeMatrixColumns :: Matrix -> MatrixNormalization
normalizeMatrixColumns m =
    let (columns, ranges, mins) = foldr
            (\c (columns, ranges, mins) -> let ColumnNormalization{..} = normalizeColumn m c in (cnValues : columns, cnRange : ranges, cnMin : mins))
            ([], [], [])
            [0..cols m - 1]
    in MatrixNormalization (fromColumns columns) ranges mins
