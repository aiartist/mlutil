{-# LANGUAGE RecordWildCards #-}

module GradientAscentDemo (runGradientAscentDemos) where

import           Ch05LogisticRegression.GradientAscent
import           DataFiles
import qualified Data.Vector.Unboxed as VU
import           MLUtil
import           MLUtil ((|||))
import           MLUtil.Graphics hiding ((|||))
import           Graphics.Rendering.Chart.Easy

waveform :: (Double -> Double) -> [Double] -> [(Double, Double)]
waveform f xs = [ (x, f x) | x <- xs ]

createSigmoidFigures :: IO ()
createSigmoidFigures = do
    renderChartSVG
        "sigmoid-fig5-1a.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.1a: Sigmoid"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "sigmoid(x)"
        }
        [ mkRPlot (line "sigmoid(x)" [waveform sigmoid [-5.0, -4.9 .. 5.0]]) ]
    renderChartSVG
        "sigmoid-fig5-1b.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.1b: Sigmoid (zoomed out)"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "sigmoid(x)"
        }
        [ mkRPlot (line "sigmoid(x)" [waveform sigmoid [-60.0, -59.0 .. 60.0]]) ]

testGradAscent :: IO ()
testGradAscent = do
    Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues) 1 ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r = gradAscent 0.001 500 values labels
    print r

runGradientAscentDemos :: IO ()
runGradientAscentDemos = do
    createSigmoidFigures
    testGradAscent
