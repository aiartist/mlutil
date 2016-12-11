{-|
Module      : Numeric.LinearAlgebra.Easy.Basics
Description : Easy linear algebra: common functions
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE FlexibleContexts #-}

module Numeric.LinearAlgebra.Easy.Basics
    ( (|>)
    , (><)
    , (<>)
    , (#>)
    , (<#)
    , asColumn
    , asRow
    , atIndex
    ) where

import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Easy.Types

-- |Create a vector from a list of elements and explicit dimension. The input
-- list is truncated if it is too long, so it may safely be used, for instance,
-- with infinite lists.
(|>) :: Int -> [R] -> Vector
infixl 9 |>
(|>) = (LA.|>)

-- |Create a matrix from a list of elements
(><) :: Int -> Int -> [R] -> Matrix
(><) = (LA.><)

-- |Dense matrix product
infixr 8 <>
(<>) :: Matrix -> Matrix -> Matrix
(<>) = (LA.<>)

-- |Dense matrix-vector product
infixr 8 #>
(#>) :: Matrix -> Vector -> Vector
(#>) = (LA.#>)

-- |Dense vector-matrix product
infixl 8 <#
(<#) :: Vector -> Matrix -> Vector
(<#) = (LA.<#)

-- |Creates a 1-row matrix from a vector
asRow :: Vector -> Matrix
asRow = LA.asRow

-- |Creates a 1-column matrix from a vector
asColumn :: Vector -> Matrix
asColumn = LA.asColumn

-- | generic indexing function
--
-- >>> vector [1,2,3] `atIndex` 1
-- 2.0
--
-- >>> matrix 3 [0..8] `atIndex` (2,0)
-- 6.0
--
atIndex :: LA.Container c R => c R -> LA.IndexOf c -> R
atIndex = LA.atIndex
