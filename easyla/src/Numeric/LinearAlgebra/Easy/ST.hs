{-|
Module      : Numeric.LinearAlgebra.Easy.ST
Description : Easy linear algebra: build vectors and matrices in ST monad
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Typical usage is to import the top-level module unqualified using @import Numeric.LinearAlgebra.Easy@
-}

{-# LANGUAGE Rank2Types #-}

module Numeric.LinearAlgebra.Easy.ST
    ( STMatrix
    , modifyMatrix
    , newMatrix
    , readMatrix
    , runSTMatrix
    , withSTMatrix
    , writeMatrix
    ) where

import           Control.Monad.ST
import qualified Numeric.LinearAlgebra.Devel as LAD
import           Numeric.LinearAlgebra.Easy.Types

type STMatrix s = LAD.STMatrix s R

-- |Creates a new mutable matrix of doubles
newMatrix :: R -> Int -> Int -> ST s (STMatrix s)
newMatrix = LAD.newMatrix

-- |Safely freezes converts a mutable matrix of doubles
runSTMatrix :: (forall s . ST s (STMatrix s)) -> Matrix
runSTMatrix = LAD.runSTMatrix

-- |Wraps operations on a mutable matrix of doubles
--
-- >>> :{
-- withSTMatrix 9 3 3 $ \m' -> do
--     writeMatrix m' 0 0 10.0
--     writeMatrix m' 1 1 20.0
--     writeMatrix m' 2 2 30.0
-- :}
-- (3><3)
--  [ 10.0,  9.0,  9.0
--  ,  9.0, 20.0,  9.0
--  ,  9.0,  9.0, 30.0 ]
--
withSTMatrix :: R -> Int -> Int -> (forall s . STMatrix s -> ST s ()) -> Matrix
withSTMatrix value m n f = runSTMatrix $ do
    x <- newMatrix value m n
    f x
    return x

-- |Applies a function to a value in a mutable matrix of doubles
modifyMatrix :: STMatrix s -> Int -> Int -> (R -> R) -> ST s ()
modifyMatrix = LAD.modifyMatrix

-- |Reads a value from a mutable matrix of doubles
readMatrix :: STMatrix s -> Int -> Int -> ST s R
readMatrix = LAD.readMatrix

-- |Writes a value to a mutable matrix of doubles
writeMatrix :: STMatrix s -> Int -> Int -> R -> ST s ()
writeMatrix = LAD.writeMatrix
