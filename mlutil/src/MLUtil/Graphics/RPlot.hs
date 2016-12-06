{-# LANGUAGE ExistentialQuantification #-}

module MLUtil.Graphics.RPlot
    ( RPlot ()
    , mkRPlot
    , plotRPlot
    ) where

import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports

-- |Plot
data RPlot = forall p . ToPlot p => RPlot (EC (Layout R R) (p R R))

-- |Wraps plot for use with renderChartSVG
mkRPlot :: ToPlot p => EC (Layout R R) (p R R) -> RPlot
mkRPlot = RPlot

-- |Plot wrapped plot object
plotRPlot :: RPlot -> EC (Layout R R) ()
plotRPlot (RPlot p) = plot p
