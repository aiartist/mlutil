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
    , ident
    , matrix
    , vector
    ) where

import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra.Easy.Types

-- |Creates a vector of doubles from a list of elements and explicit dimension.
-- The input list is truncated if it is too long, so it may safely be used, for
-- instance, with infinite lists.
--
-- >>> 5 |> [1..]
--[1.0,2.0,3.0,4.0,5.0]
--
(|>) :: Int -> [R] -> Vector
infixl 9 |>
(|>) = (LA.|>)

-- |Creates a vector of doubles
--
-- >>> vector [1..5]
-- [1.0,2.0,3.0,4.0,5.0]
--
vector :: [R] -> Vector
vector = LA.vector

-- |Creates a matrix of doubles from a list of elements
--
-- >>> (2 >< 3) [2, 4, 7, -3, 11, 0]
-- (2><3)
--  [  2.0,  4.0, 7.0
--  , -3.0, 11.0, 0.0 ]
--
-- The input list is explicitly truncated, so that it can safely be used with
-- lists that are too long (like infinite lists).
--
-- >>> (2 >< 3) [1..]
-- (2><3)
--  [ 1.0, 2.0, 3.0
--  , 4.0, 5.0, 6.0 ]
--
-- This is the format produced by the instances of Show (Matrix), which can also
-- be used for input.
--
(><) :: Int -> Int -> [R] -> Matrix
(><) = (LA.><)

-- |Creates a matrix of doubles
--
-- >>> matrix 5 [1..15]
-- (3><5)
--  [  1.0,  2.0,  3.0,  4.0,  5.0
--  ,  6.0,  7.0,  8.0,  9.0, 10.0
--  , 11.0, 12.0, 13.0, 14.0, 15.0 ]
--
matrix :: Int -> [R] -> Matrix
matrix = LA.matrix

-- |Dense matrix product
--
-- >>> a = (3 >< 5) [1..]
-- >>> a
-- (3><5)
--  [  1.0,  2.0,  3.0,  4.0,  5.0
--  ,  6.0,  7.0,  8.0,  9.0, 10.0
--  , 11.0, 12.0, 13.0, 14.0, 15.0 ]
--
-- >>> b = (5 >< 2) [1, 3, 0, 2, -1, 5, 7, 7, 6, 0]
-- >>> b
-- (5><2)
--  [  1.0, 3.0
--  ,  0.0, 2.0
--  , -1.0, 5.0
--  ,  7.0, 7.0
--  ,  6.0, 0.0 ]
--
-- >>> a <> b
-- (3><2)
--  [  56.0,  50.0
--  , 121.0, 135.0
--  , 186.0, 220.0 ]
--
infixr 8 <>
(<>) :: Matrix -> Matrix -> Matrix
(<>) = (LA.<>)

-- |Dense matrix-vector product
--
-- >>> m = (2 >< 3) [1..]
-- >>> m
-- (2><3)
--  [ 1.0, 2.0, 3.0
--  , 4.0, 5.0, 6.0 ]
--
-- >>> v = vector [10, 20, 30]
-- >>> v
-- [10.0,20.0,30.0]
--
-- >>> m #> v
-- [140.0,320.0]
--
infixr 8 #>
(#>) :: Matrix -> Vector -> Vector
(#>) = (LA.#>)

-- |Dense vector-matrix product
--
-- >>> v = vector [10, 20, 30]
-- >>> v
-- [10.0,20.0,30.0]
--
-- >>> m = (3 >< 2) [1..]
-- >>> m
-- (3><2)
--  [ 1.0, 2.0
--  , 3.0, 4.0
--  , 5.0, 6.0 ]
--
-- >>> v <# m
-- [220.0,280.0]
--
infixl 8 <#
(<#) :: Vector -> Matrix -> Vector
(<#) = (LA.<#)

-- |Creates a 1-row matrix from a vector
--
-- >>> asRow (vector [1..5])
-- (1><5)
--  [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
--
asRow :: Vector -> Matrix
asRow = LA.asRow

-- |Creates a 1-column matrix from a vector
--
-- >>> asColumn (vector [1..5])
-- (5><1)
--  [ 1.0
--  , 2.0
--  , 3.0
--  , 4.0
--  , 5.0 ]
--
asColumn :: Vector -> Matrix
asColumn = LA.asColumn

-- |Generic indexing function
--
-- >>> vector [1,2,3] `atIndex` 1
-- 2.0
--
-- >>> matrix 3 [0..8] `atIndex` (2,0)
-- 6.0
--
atIndex :: LA.Container c R => c R -> LA.IndexOf c -> R
atIndex = LA.atIndex

-- |Creates the identity matrix of doubles of given dimension
--
-- >>> m = ident 3
-- >>> m
-- (3><3)
--  [ 1.0, 0.0, 0.0
--  , 0.0, 1.0, 0.0
--  , 0.0, 0.0, 1.0 ]
--
ident :: Int -> Matrix
ident = LA.ident
