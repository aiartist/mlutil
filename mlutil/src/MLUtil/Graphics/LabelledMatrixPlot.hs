{-# LANGUAGE RecordWildCards #-}

module MLUtil.Graphics.LabelledMatrixPlot
    ( colouredSeriesPlots
    , simplePlot
    ) where

import qualified Data.Map as M
import           Data.Vector.Storable as VS hiding (foldr, map)
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.Chart.Easy (points)
import           MLUtil.Imports
import           MLUtil.Graphics.RPlot
import           MLUtil.LabelledMatrix

type Coordinate = (R, R)
type CoordinateList = [Coordinate]
type PlotSpec = (String, CoordinateList)

partitionIndices :: Ord a => [a] -> M.Map a [Int]
partitionIndices xs = foldr f M.empty (zip [0..] xs)
    where f (i, x) m = M.alter (\mb -> case mb of Nothing -> Just [i]; Just is -> Just (i : is)) x m

colouredSeriesPlotSpecs :: LabelledMatrix -> Int -> Int -> [PlotSpec]
colouredSeriesPlotSpecs LabelledMatrix{..} xColumnIndex yColumnIndex =
    let columns = toColumns lmValues
        xColumn = columns !! xColumnIndex
        yColumn = columns !! yColumnIndex
        labelIds = VU.toList lmLabelIds
        partitions = M.toList $ partitionIndices labelIds
    in (flip map) partitions $ \(labelId, indices) ->
        let subseries = foldr f [] indices
                          where f i cs = let c = ((VS.!) xColumn i, (VS.!) yColumn i) in c : cs
            Just labelText = M.lookup labelId lmLabelMap
        in (labelText, subseries)

colouredSeriesPlots :: LabelledMatrix -> Int -> Int -> [RPlot]
colouredSeriesPlots m xColumnIndex yColumnIndex = map
    (mkRPlot . uncurry points)
    (colouredSeriesPlotSpecs m xColumnIndex yColumnIndex)

simplePlot :: LabelledMatrix -> Int -> Int -> RPlot
simplePlot LabelledMatrix{..} xColumnIndex yColumnIndex =
    let columns = toColumns lmValues
        xColumn = columns !! xColumnIndex
        yColumn = columns !! yColumnIndex
    in mkRPlot $ points "Series" (zip (VS.toList xColumn) (VS.toList yColumn))
