{-# LANGUAGE RecordWildCards #-}

module GradientAscentDemo (runGradientAscentDemos) where

import           Ch05LogisticRegression.GradientAscent
import           Control.Monad
import           DataFiles
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           MLUtil
import           MLUtil ((|||))
import           MLUtil.Graphics hiding ((|||), Vector)
import           Graphics.Rendering.Chart.Easy hiding (Matrix, Vector)

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
    let values = ones (rows lmValues, 1) ||| lmValues
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
    let values = ones (rows lmValues, 1) ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r = stocGradAscent0 0.01 values labels
        plots = (mkRPlot $ line "best-fit line" [waveform (bestFitLine r) [-3.0, -2.9 .. 3.0]])
                    : (colouredSeriesPlots m 0 1)
    renderChartSVG
        "sigmoid-fig5-5.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.5: Stochastic gradient ascent best-fit line"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "y"
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

    let values = ones (rows lmValues, 1) ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        rowCount = rows values
        Just eiAction = choiceExtractIndices rowCount rowCount
        numIter = 150
    eis <- foldM (\eis _ -> eiAction >>= \ei -> return $ ei : eis) [] [1 .. numIter]
    let r = stocGradAscent1 0.01 values labels eis
        plots = (mkRPlot $ line "best-fit line" [waveform (bestFitLine r) [-3.0, -2.9 .. 3.0]])
                    : (colouredSeriesPlots m 0 1)
    renderChartSVG
        "sigmoid-fig5-8.svg"
        defaultChartLabels
        { clTitle = Just "Figure 5.8: Improved stochastic gradient ascent best-fit line"
        , clXAxisLabel = Just "x"
        , clYAxisLabel = Just "y"
        }
        plots

-- cf logRegres.plotBestFit
bestFitLine :: Matrix -> Double -> Double
bestFitLine weights x =
    let w0 = weights `atIndex` (0, 0)
        w1 = weights `atIndex` (1, 0)
        w2 = weights `atIndex` (2, 0)
    in ((-w0) - w1 * x) / w2

testGradAscent :: IO ()
testGradAscent = do
    Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues, 1) ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r = gradAscent 0.001 500 values labels
    print r

testStocGradAscent :: IO ()
testStocGradAscent = do
    Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
    let values = ones (rows lmValues, 1) ||| lmValues
        labels = col (map fromIntegral (VU.toList lmLabelIds))
        r0 = stocGradAscent0 0.01 values labels
        rowCount = rows values
        Just eiAction = choiceExtractIndices rowCount rowCount
        numIter = 150
    eis <- foldM (\eis _ -> eiAction >>= \ei -> return $ ei : eis) [] [1 .. numIter]
    let r1 = stocGradAscent1 0.01 values labels eis
    print r0
    print r1

-- cf logRegres.colicTest
-- I'm bored of this now
-- This function doesn't work
colicTest :: IO ()
colicTest = do
    Just trainingM <- getDataFileName "horseColicTraining.txt" >>= readLabelledMatrix
    let trainingValues = lmValues trainingM
        trainingLabels = col (map fromIntegral (VU.toList (lmLabelIds trainingM)))
        trainingRowCount = rows trainingValues
        Just eiAction = choiceExtractIndices trainingRowCount trainingRowCount
    eis <- foldM (\eis _ -> eiAction >>= \ei -> return $ ei : eis) [] [1 .. 500]
    let weights = flatten $ stocGradAscent1 0.01 trainingValues trainingLabels eis

    Just testM <- getDataFileName "horseColicTest.txt" >>= readLabelledMatrix
    let testValues = lmValues testM
        testLabels = VU.toList (lmLabelIds testM)
        testRows = toRows testValues
        (n, err) = foldr
                    (\(testRow, testLabel) (n, err) ->
                        let r = classifyVector testRow weights;
                            err' = if r == testLabel then err else err + 1
                        in (n + 1, err'))
                    (0, 0)
                    (zip testRows testLabels)
        errorRate = (fromIntegral err) / (fromIntegral n)
    putStrLn $ "errorRate=" ++ show errorRate

-- cf logRegres.classifyVector
classifyVector :: Vector -> Vector -> Int
classifyVector inX weights =
    let prob = sigmoid (sumElements (inX * weights))
    in if prob > 0.5
        then 1
        else 0

runGradientAscentDemos :: IO ()
runGradientAscentDemos = do
    createSigmoidFigures
    testGradAscent
    testStocGradAscent
    colicTest
