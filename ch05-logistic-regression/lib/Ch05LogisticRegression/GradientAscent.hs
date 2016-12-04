module Ch05LogisticRegression.GradientAscent
    ( gradAscent
    , sigmoid
    ) where

-- cf logRegres.sigmoid
sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

-- cf logRegres.gradAscent
gradAscent :: IO ()
gradAscent = return ()
