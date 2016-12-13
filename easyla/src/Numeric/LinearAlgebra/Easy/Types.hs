{-|
Module      : Numeric.LinearAlgebra.Easy.Types
Description : Easy linear algebra: common types
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Commonly used types and type aliases

Typical usage is to import the top-level module unqualified using @import Numeric.LinearAlgebra.Easy@
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

module Numeric.LinearAlgebra.Easy.Types
    ( Matrix
    , Ones
    , R
    , Vector
    , Zeros
    , ones
    , zeros
    ) where

import qualified Numeric.LinearAlgebra as LA

-- |Two-dimensional matrix of @Double@ values
type Matrix = LA.Matrix R

-- |@Double@ numeric data type
type R = LA.R

-- |Row or column vector of @Double@ values
type Vector = LA.Vector R

-- |Types that define an all-ones instance
class Ones d c | c -> d where
    -- |All-ones instance of size @d@
    ones :: d -> c

instance Ones Int Vector where ones = LA.konst 1
instance Ones (Int, Int) Matrix where ones = LA.konst 1

-- |Types that define an all-zeroes instance
class Zeros d c | c -> d where
    -- |All-zeroes instance of size @d@
    zeros :: d -> c

instance Zeros Int Vector where zeros = LA.konst 0
instance Zeros (Int, Int) Matrix where zeros = LA.konst 0
