{-# LANGUAGE RecordWildCards #-}

module MLUtil.Graphics.Rendering
    ( ChartLabels (..)
    , defaultChartLabels
    , renderChartSVG
    , renderFlowchartSVG
    ) where

import           Diagrams.Backend.SVG
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           MLUtil.Graphics.Flowchart
import           MLUtil.Graphics.Imports

data ChartLabels = ChartLabels
    { clTitle :: Maybe String
    , clXAxisLabel :: Maybe String
    , clYAxisLabel :: Maybe String
    } deriving Show

defaultChartLabels :: ChartLabels
defaultChartLabels = ChartLabels
    { clTitle = Nothing
    , clXAxisLabel = Nothing
    , clYAxisLabel = Nothing
    }

renderChartSVG :: (PlotValue a, ToPlot p) => FilePath -> ChartLabels -> [EC (Layout a a) (p a a)] -> IO ()
renderChartSVG path ChartLabels{..} ps = toFile def path $ do
    let setMaybe p (Just x) = p .= x
        setMaybe p Nothing = return ()
    setMaybe layout_title clTitle
    setMaybe (layout_x_axis . laxis_title) clXAxisLabel
    setMaybe (layout_y_axis . laxis_title) clYAxisLabel
    mapM_ plot ps

renderFlowchartSVG :: FilePath -> Flowchart -> IO ()
renderFlowchartSVG path = renderSVG path (mkWidth 500)
