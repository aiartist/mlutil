{-# LANGUAGE RecordWildCards #-}

module MLUtil.Charting
    ( ChartLabels (..)
    , defaultChartLabels
    , renderChartSVG
    , renderFlowchartSVG
    ) where

import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports
import           MLUtil.RRScatterPlot

data ChartLabels = ChartLabels
    { clTitle :: Maybe String
    , clXAxisLabel :: Maybe String
    , clYAxisLabel :: Maybe String
    } deriving Show

type Flowchart = Diagram B

defaultChartLabels :: ChartLabels
defaultChartLabels = ChartLabels Nothing Nothing Nothing

renderChartSVG :: FilePath -> ChartLabels ->[RRScatterPlot] -> IO ()
renderChartSVG path ChartLabels{..} ps = toFile def path $ do
    let setMaybe p (Just x) = p .= x
        setMaybe p Nothing = return ()
    setMaybe layout_title clTitle
    setMaybe (layout_x_axis . laxis_title) clXAxisLabel
    setMaybe (layout_y_axis . laxis_title) clYAxisLabel
    mapM_ plot ps

renderFlowchartSVG :: FilePath -> Flowchart -> IO ()
renderFlowchartSVG path = renderSVG path (mkWidth 500)
