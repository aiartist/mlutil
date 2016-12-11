{-|
Module      : LA
Description : Richard's linear algebra library
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Sits on top of hmatrix and vector

Goals:

* Expose mostly monomorphic versions of commonly used functions
* Simplify where appropriate
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

module LA
    ( (|>)
    , (><)
    , (<>)
    , (#>)
    , (<#)
    , Matrix
    , R
    , STMatrix
    , Vector
    , asColumn
    , asRow
    , newMatrix
    , ones
    , runSTMatrix
    , withSTMatrix
    , writeMatrix
    , zeroes
    ) where

import           Control.Monad.ST
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Devel as NLAD

type Matrix = NLA.Matrix R
type R = NLA.R
type STMatrix s = NLAD.STMatrix s R
type Vector = NLA.Vector R

class Ones d c | c -> d where ones :: d -> c
instance Ones Int Vector where ones = NLA.konst 1
instance Ones (Int, Int) Matrix where ones = NLA.konst 1

class Zeroes d c | c -> d where zeroes :: d -> c
instance Zeroes Int Vector where zeroes = NLA.konst 0
instance Zeroes (Int, Int) Matrix where zeroes = NLA.konst 0

-- |Create a vector from a list of elements and explicit dimension. The input
-- list is truncated if it is too long, so it may safely be used, for instance,
-- with infinite lists.
(|>) :: Int -> [R] -> Vector
infixl 9 |>
(|>) = (NLA.|>)

-- |Create a matrix from a list of elements
(><) :: Int -> Int -> [R] -> Matrix
(><) = (NLA.><)

-- |Dense matrix product
infixr 8 <>
(<>) :: Matrix -> Matrix -> Matrix
(<>) = (NLA.<>)

-- |Dense matrix-vector product
infixr 8 #>
(#>) :: Matrix -> Vector -> Vector
(#>) = (NLA.#>)

-- |Dense vector-matrix product
infixl 8 <#
(<#) :: Vector -> Matrix -> Vector
(<#) = (NLA.<#)

-- |Creates a 1-row matrix from a vector
asRow :: Vector -> Matrix
asRow = NLA.asRow

-- |Creates a 1-column matrix from a vector
asColumn :: Vector -> Matrix
asColumn = NLA.asColumn

newMatrix :: R -> Int -> Int -> ST s (STMatrix s)
newMatrix = NLAD.newMatrix

runSTMatrix :: (forall s . ST s (STMatrix s)) -> Matrix
runSTMatrix = NLAD.runSTMatrix

writeMatrix :: STMatrix s -> Int -> Int -> R -> ST s ()
writeMatrix = NLAD.writeMatrix

withSTMatrix :: R -> Int -> Int -> (forall s . STMatrix s -> ST s ()) -> Matrix
withSTMatrix value m n f = runSTMatrix $ do
    x <- newMatrix value m n
    f x
    return x
