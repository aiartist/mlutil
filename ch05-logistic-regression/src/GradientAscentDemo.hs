{-# LANGUAGE RecordWildCards #-}

module GradientAscentDemo (runGradientAscentDemos) where

import           Ch05LogisticRegression.GradientAscent
import           Control.Monad
import           DataFiles
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           MLUtil
import           MLUtil ((|||))
import           MLUtil.Graphics hiding ((|||))
import           Graphics.Rendering.Chart.Easy hiding (Matrix)

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

    Just m@LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues) 1 ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r = gradAscent 0.001 500 values labels
        plots = (mkRPlot $ line "best-fit line" [waveform (bestFitLine r) [-3.0, -2.9 .. 3.0]])
                    : (colouredSeriesPlots m 0 1)
    renderChartSVG
        "sigmoid-fig5-4.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.4: Logistic regression best-fit line after gradient ascent"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "sigmoid(x)"
        }
        plots

    Just m@LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues) 1 ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r = stocGradAscent0 0.01 values labels
        plots = (mkRPlot $ line "best-fit line" [waveform (bestFitLine r) [-3.0, -2.9 .. 3.0]])
                    : (colouredSeriesPlots m 0 1)
    renderChartSVG
        "sigmoid-fig5-5.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.5: Stochastic gradient ascent best-fit line"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "sigmoid(x)"
        }
        plots

    -- The performance-related failings of my implementation become apparent
    -- when trying to record the history: running to 500 iterations like Peter's
    -- sample is not really viable!
    -- TODO: Optimize!
    let (_, history) = stocGradAscent0History 0.01 50 values labels
        plots = map (\i -> mkRPlot $ line ("x" ++ show i) [zip (map fromIntegral [0 ..]) (map (`atIndex` i) history)]) [0 .. 2]
    renderChartSVG
        "sigmoid-fig5-6.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.6: Weights vs. iteration number"
        , clXAxisLabel = Just "Iteration"
        , clYAxisLabel = Just "Weights"
        }
        plots

-- cf logRegres.plotBestFit
bestFitLine :: Matrix R -> Double -> Double
bestFitLine weights x =
    let w0 = weights `atIndex` (0, 0)
        w1 = weights `atIndex` (1, 0)
        w2 = weights `atIndex` (2, 0)
    in ((-w0) - w1 * x) / w2

testGradAscent :: IO ()
testGradAscent = do
    Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues) 1 ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r = gradAscent 0.001 500 values labels
    print r

testStocGradAscent :: IO ()
testStocGradAscent = do
    Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues) 1 ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r0 = stocGradAscent0 0.01 values labels
        rowCount = rows values
        Just eiAction = choiceExtractIndices rowCount rowCount
        numIter = 150
    eis <- foldM (\eis _ -> eiAction >>= \ei -> return $ ei : eis) [] [1 .. numIter]
    let r1 = stocGradAscent1 0.01 values labels eis
    --print r0
    print r1

runGradientAscentDemos :: IO ()
runGradientAscentDemos = do
    --createSigmoidFigures
    --testGradAscent
    testStocGradAscent
