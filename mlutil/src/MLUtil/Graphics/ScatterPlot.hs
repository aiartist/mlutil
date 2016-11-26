module MLUtil.Graphics.ScatterPlot
    ( ScatterPlot
    ) where

import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports

type ScatterPlot = EC (Layout R R) (PlotPoints R R)
