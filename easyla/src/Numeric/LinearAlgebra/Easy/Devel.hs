{-|
Module      : Numeric.LinearAlgebra.Easy.Devel
Description : Easy linear algebra: advanced operations
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Low-level API

Typical usage is to import the top-level module unqualified using @import Numeric.LinearAlgebra.Easy@
-}

module Numeric.LinearAlgebra.Easy.Devel
    ( LAD.MatrixOrder (..)
    , matrixFromVector
    ) where

import qualified Numeric.LinearAlgebra.Devel as LAD
import           Numeric.LinearAlgebra.Easy.Types

-- |Converts vector of @Double@ to matrix
matrixFromVector :: LAD.MatrixOrder -> Int -> Int -> Vector -> Matrix
matrixFromVector = LAD.matrixFromVector
