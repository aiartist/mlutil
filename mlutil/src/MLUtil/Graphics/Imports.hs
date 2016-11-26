module MLUtil.Graphics.Imports
    ( module Diagrams.Prelude
    , Diagram
    , FontMeasure
    , Measure
    , Point
    , toMeasure
    ) where

import qualified Diagrams.Backend.SVG as SVG
import           Diagrams.Prelude hiding (Diagram, Measure, Point)
import qualified Diagrams.Prelude as DP

type Diagram = DP.Diagram SVG.B
type FontMeasure = DP.Measured Double Double
type Measure = Double
type Point = DP.P2 Measure

toMeasure :: Integral a => a -> Measure
toMeasure = fromIntegral
