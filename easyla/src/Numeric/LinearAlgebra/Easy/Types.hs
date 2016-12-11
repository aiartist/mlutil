{-|
Module      : Numeric.LinearAlgebra.Easy.Types
Description : Easy linear algebra: common types
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
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
    ) where

import qualified Numeric.LinearAlgebra as LA

type Matrix = LA.Matrix R
type R = LA.R
type Vector = LA.Vector R

class Ones d c | c -> d where ones :: d -> c
instance Ones Int Vector where ones = LA.konst 1
instance Ones (Int, Int) Matrix where ones = LA.konst 1

class Zeros d c | c -> d where zeros :: d -> c
instance Zeros Int Vector where zeros = LA.konst 0
instance Zeros (Int, Int) Matrix where zeros = LA.konst 0
