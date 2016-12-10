module Ch05LogisticRegression.GradientAscent
    ( gradAscent
    , sigmoid
    , stocGradAscent0
    , stocGradAscent0History
    , stocGradAscent1
    ) where

import qualified Data.Vector.Storable as VS
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
            let r = rows' !! i -- Bad, bad, bad
                label = labels `atIndex` (i, 0)
                h = sigmoid $ sumElements (mulElements r weights)
                err = label - h
                weightsDelta = scale (alpha * err) r
            in addElements weights weightsDelta)
        (VS.replicate n 1) -- initial weights
        [m - 1, m - 2 .. 0])

-- stocGradAscent0 modified to allow us to track history of weights
stocGradAscent0History :: Double -> Int -> Matrix R -> Matrix R -> (VS.Vector R, [VS.Vector R])
stocGradAscent0History alpha n values labels = foldl -- TODO: Can we improve on this?
    (\p _ -> helper alpha p values labels)
    (VS.replicate (rows values) 1, [])
    [1 .. n]

helper :: Double -> (VS.Vector R, [VS.Vector R]) -> Matrix R -> Matrix R -> (VS.Vector R, [VS.Vector R])
helper alpha p values labels =
    let m = rows values
        n = cols values
        rows' = toRows values -- Bad, bad, bad
    in foldl -- VS.toList is bad, bad, bad
        (\(weights, history) i ->
            let r = rows' !! i -- Bad, bad, bad
                label = labels `atIndex` (i, 0)
                h = sigmoid $ sumElements (mulElements r weights)
                err = label - h
                weightsDelta = scale (alpha * err) r
                weights' = addElements weights weightsDelta
            in (weights', history ++ [weights'])) -- TODO: Tweak fold since concatenation is O(N) I think
        p
        [0 .. m - 1]

stocGradAscent1 :: Double -> Matrix R -> Matrix R -> [ExtractIndices] -> Matrix R
stocGradAscent1 alpha values labels eis =
    let m = rows values
        n = cols values
        rows' = toRows values -- Bad, bad, bad
        labels' = map (\i -> labels `atIndex` (i, 0)) [0 .. m - 1]
    in col $ VS.toList -- VS.toList is bad, bad, bad
        (foldl
            (\w (j, ei) -> stocGradAscent1Helper rows' labels' j ei w)
            (VS.replicate n 1)
            (zip [0 ..] eis))

stocGradAscent1Helper :: [VS.Vector R] -> [R] -> Int -> ExtractIndices -> VS.Vector R -> VS.Vector R
stocGradAscent1Helper rows'' labels'' j ei weights =
    let Just (_, rows') = extract ei (zip rows'' labels'')
    in foldl
    (\weights (i, (r, label)) ->
        let alpha = 4.0 / (1.0 + fromIntegral (j + i)) + 0.0001
            h = sigmoid $ sumElements (mulElements r weights)
            err = label - h
            weightsDelta = scale (alpha * err) r
        in addElements weights weightsDelta)
    weights
    (zip [0 ..] rows')

mulElements :: VS.Vector R -> VS.Vector R -> VS.Vector R
mulElements = VS.zipWith (*)

addElements :: VS.Vector R -> VS.Vector R -> VS.Vector R
addElements = VS.zipWith (+)
