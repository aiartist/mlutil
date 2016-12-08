module Ch05LogisticRegression.GradientAscent
    ( gradAscent
    , sigmoid
    , stocGradAscent0
    ) where

import qualified Data.Vector.Storable as VS
import           Debug.Trace
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

-- cf logRegres.stocGradAscent0
-- This function demonstrates why we need to build a consistent set of
-- primitives for linear algebra...
-- TODO: Eliminate all the unnecessary conversions to and from lists etc.
stocGradAscent0 :: Double -> Matrix R -> Matrix R -> Matrix R
stocGradAscent0 alpha values labels =
    let m = rows values
        n = cols values
        rows' = toRows values -- Bad, bad, bad
    in col $ VS.toList (foldr -- VS.toList is bad, bad, bad
        (\i weights ->
            let rTemp = rows' !! i -- Bad, bad, bad
                r = trace ("rTemp=" ++ show rTemp) rTemp
                label = labels `atIndex` (i, 0)
                h = sigmoid $ sumElements (mulElements r weights)
                err = label - h
                weightsDelta = scale (alpha * err) r
            in addElements weights weightsDelta)
        (VS.replicate n 1) -- initial weights
        [m - 1, m - 2 .. 0])

mulElements :: VS.Vector R -> VS.Vector R -> VS.Vector R
mulElements = VS.zipWith (*)

addElements :: VS.Vector R -> VS.Vector R -> VS.Vector R
addElements = VS.zipWith (+)
