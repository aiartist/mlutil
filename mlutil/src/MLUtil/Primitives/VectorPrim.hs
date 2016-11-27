module MLUtil.Primitives.VectorPrim
    ( addS
    , addV
    , divV
    , floatV
    , logV
    , mulV
    , sumV
    ) where

import qualified Data.Vector as V

-- |Result of adding scalar to vector
addS :: Num a => a -> V.Vector a -> V.Vector a
addS x = V.map (+ x)

-- |Result of adding two vectors
addV :: Num a => V.Vector a -> V.Vector a -> V.Vector a
addV = V.zipWith (+)

-- |Result of elementwise division of vector into another vector
divV :: (Integral a, Fractional b) => V.Vector a -> V.Vector a -> V.Vector b
divV xs ys = V.zipWith (\x y -> fromIntegral x / fromIntegral y) xs ys

-- |Result of conversion of vector elements from integral
floatV :: (Integral a, Num b) => V.Vector a -> V.Vector b
floatV = V.map fromIntegral

-- |Result of apply log function to elements of vector
logV :: Floating a => V.Vector a -> V.Vector a
logV = V.map log

-- |Result of elementwise multiplication of two vectors
mulV :: Num a => V.Vector a -> V.Vector a -> V.Vector a
mulV = V.zipWith (*)

-- |Result of summing elements of vector
sumV :: Num a => V.Vector a -> a
sumV = V.foldr1 (+)
