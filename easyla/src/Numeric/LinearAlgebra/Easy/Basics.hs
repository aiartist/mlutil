{-|
Module      : Numeric.LinearAlgebra.Easy.Basics
Description : Easy linear algebra: common functions
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Typical usage is to import the top-level module unqualified using @import Numeric.LinearAlgebra.Easy@
-}

{-# LANGUAGE FlexibleContexts #-}

module Numeric.LinearAlgebra.Easy.Basics
    ( LA.scale -- TODO: Wrap this to make it less polymorphic!
    , (LA.|||) -- TODO: Wrap this to make it less polymorphic!
    , (|>)
    , (><)
    , (<>)
    , (#>)
    , (<#)
    , asColumn
    , asRow
    , atIndex
    , cmap
    , col
    , cols
    , flatten
    , fromColumns
    , fromLists
    , fromRows
    , ident
    , matrix
    , prodElements
    , repmat
    , row
    , rows
    , scalar
    , size
    , subMatrix
    , sumElements
    , toColumns
    , toLists
    , toRows
    , tr'
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

-- |Creates a matrix of doubles from a list of vectors as columns
-- >>> fromColumns [vector [1.0, 4.0, 7.0], vector [2.0, 5.0, 8.0], vector [3.0, 6.0, 9.0]]
-- (3><3)
--  [ 1.0, 2.0, 3.0
--  , 4.0, 5.0, 6.0
--  , 7.0, 8.0, 9.0 ]
--
fromColumns :: [Vector] -> Matrix
fromColumns = LA.fromColumns

-- |Extracts the columns of a matrix as a list of vectors of double
--
-- >>> m = (3 >< 3) [1..]
-- >>> toColumns m
-- [[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0]]
--
toColumns :: Matrix -> [Vector]
toColumns = LA.toColumns

-- |Creates a matrix of doubles from a list of vectors as rows
-- >>> fromRows [vector [1.0, 2.0, 3.0], vector [4.0, 5.0, 6.0], vector [7.0, 8.0, 9.0]]
-- (3><3)
--  [ 1.0, 2.0, 3.0
--  , 4.0, 5.0, 6.0
--  , 7.0, 8.0, 9.0 ]
--
fromRows :: [Vector] -> Matrix
fromRows = LA.fromRows

-- |Extracts the rows of a matrix as a list of vectors of double
--
-- >>> m = (3 >< 3) [1..]
-- >>> toRows m
-- [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]
--
toRows :: Matrix -> [Vector]
toRows = LA.toRows

-- |Maps function of R -> R over elements of container
--
-- >>> cmap (+ 10.0) (3 |> [1..])
-- [11.0,12.0,13.0]
-- >>> cmap (+ 10.0) ((3 >< 3) [1..])
-- (3><3)
--  [ 11.0, 12.0, 13.0
--  , 14.0, 15.0, 16.0
--  , 17.0, 18.0, 19.0 ]
--
cmap :: LA.Container c R => (R -> R) -> c R -> c R
cmap = LA.cmap

-- |Size of vector or matrix of doubles
-- >>> size $ vector [1..10]
-- 10
--  >>> size $ (2 >< 5) [1..]
--(2,5)
--
size :: LA.Container c R => c R -> LA.IndexOf c
size = LA.size

-- |Number of columns in matrix of doubles
--
-- >>> cols ((10 >< 20) [1..])
-- 20
--
cols :: Matrix -> Int
cols = LA.cols

-- |Number of rows in matrix of doubles
--
-- >>> rows ((10 >< 20) [1..])
-- 10
--
rows :: Matrix -> Int
rows = LA.rows

-- |Transpose matrix of doubles
--
-- >>> tr' $ (3 >< 2) [1..]
-- (2><3)
--  [ 1.0, 3.0, 5.0
--  , 2.0, 4.0, 6.0 ]
--
tr' :: Matrix -> Matrix
tr' = LA.tr'

-- |Creates matrix of doubles by repetition of a matrix a given number of rows and columns
--
-- >>> repmat (ident 2) 2 3
-- (4><6)
--  [ 1.0, 0.0, 1.0, 0.0, 1.0, 0.0
--  , 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
--  , 1.0, 0.0, 1.0, 0.0, 1.0, 0.0
--  , 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 ]
--
repmat :: Matrix -> Int -> Int -> Matrix
repmat = LA.repmat

-- |Creates a vector of doubles by concatenation of rows. If the matrix is
-- column-major, this operation requires a transpose.
--
-- >>> flatten (ident 3)
-- [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]
--
flatten :: Matrix -> Vector
flatten = LA.flatten

-- |Create a single-column matrix of doubles from a list
--
-- >>> col [7, -2, 4]
-- (3><1)
--  [  7.0
--  , -2.0
--  ,  4.0 ]
--
col :: [R] -> Matrix
col = LA.col

-- |Creates a single-row matrix of doubles from a list
--
-- >>> row [2, 3, 1, 8]
-- (1><4)
--  [ 2.0, 3.0, 1.0, 8.0 ]
--
row :: [R] -> Matrix
row = LA.row


-- |Creates a vector or matrix with a single double element
--
-- >>> scalar 100.0 :: Vector
-- [100.0]
-- >>> scalar 100.0 :: Matrix
-- (1><1)
--  [ 100.0 ]
--
scalar :: LA.Container c R => R -> c R
scalar = LA.scalar

-- |Sum of elements
--
-- >>> sumElements (5 |> [1..])
-- 15.0
-- >>> sumElements ((2 >< 2) [1..])
-- 10.0
--
sumElements :: LA.Container c R => c R -> R
sumElements = LA.sumElements

-- |Product of elements
--
-- >>> prodElements (5 |> [1..])
-- 120.0
-- >>> prodElements ((2 >< 2) [1..])
-- 24.0
--
prodElements :: LA.Container c R => c R -> R
prodElements = LA.prodElements

-- |Reference to a rectangular slice of a matrix of doubles (no data copy)
--
-- >>> subMatrix (1, 1) (3, 2) ((5 >< 4) [1..])
-- (3><2)
--  [  6.0,  7.0
--  , 10.0, 11.0
--  , 14.0, 15.0 ]
--
subMatrix :: (Int, Int) -> (Int, Int) -> Matrix -> Matrix
subMatrix = LA.subMatrix

-- |Creates a matrix of doubles from a list of lists (taken as rows)
--
-- >>> fromLists [[1,2],[3,4],[5,6]]
-- (3><2)
--  [ 1.0, 2.0
--  , 3.0, 4.0
--  , 5.0, 6.0 ]
--
fromLists :: [[R]] -> Matrix
fromLists = LA.fromLists

-- |Creates a list of list of doubles from a matrix
--
-- >>> toLists ((3 >< 2) [1..])
-- [[1.0,2.0],[3.0,4.0],[5.0,6.0]]
--
toLists :: Matrix -> [[R]]
toLists = LA.toLists
