module SigmoidDemo (runSigmoidDemos) where

import           MLUtil.Graphics
import           Graphics.Rendering.Chart.Easy

waveform :: (Double -> Double) -> [Double] -> [(Double, Double)]
waveform f xs = [ (x, f x) | x <- xs ]

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

runSigmoidDemos :: IO ()
runSigmoidDemos = do
    renderChartSVG
        "sigmoid-fig5-1a.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.1a: Sigmoid"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "sigmoid(x)"
        }
        [ (line "sigmoid(x)" [ waveform sigmoid [ -5.0, -4.9 .. 5.0 ] ]) ]
    renderChartSVG
        "sigmoid-fig5-1b.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.1b: Sigmoid (zoomed out)"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "sigmoid(x)"
        }
        [ (line "sigmoid(x)" [ waveform sigmoid [ -60.0, -59.0 .. 60.0 ] ]) ]
