module Ch05LogisticRegression.GradientAscent
    ( gradAscent
    , sigmoid
    ) where

import           MLUtil

-- cf logRegres.sigmoid
sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

-- cf logRegres.gradAscent
gradAscent :: Double -> Int -> Matrix R -> Matrix R -> Matrix R
gradAscent alpha maxCycles values labels =
    let m = rows values
        n = cols values
        alpha' = scalar alpha
    in foldr
        (\_ weights ->
            let h = cmap sigmoid (values <> weights)
                err = labels - h
                weightsDelta = alpha' * (tr' values <> err)
            in weights + weightsDelta)
        (ones n 1) -- initial weights
        [0 .. maxCycles - 1]
