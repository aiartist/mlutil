{-|
Module      : Numeric.LinearAlgebra.Easy
Description : Easy linear algebra
Copyright   : (C) Richard Cook, 2016
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Simplified monomorphic bindings to <https://hackage.haskell.org/package/hmatrix hmatrix>
and other linear algebra helpers

`hmatrix` is great. However, the fact that everything is polymorphic makes it
more difficult to use than is necessary if all you ever use is @Double@. So,
this little package wraps `hmatrix` (and bits of <https://hackage.haskell.org/package/vector vector>).
Here are the design goals:

* Export simplified monomorphic versions of functions
* Specialize to @Double@
* Simplify common operations by providing helper functions
* Export all the most commonly used bits via a single namespace to enable use of
a single @import@ for common usages
* Make interface as similar as possible to existing `hmatrix` interface to aid
transition back and forth between the two

Typical usage is to import the top-level module using @import Numeric.LinearAlgebra.Easy@ unqualified
-}

module Numeric.LinearAlgebra.Easy
    ( module Numeric.LinearAlgebra.Easy.Basics
    , module Numeric.LinearAlgebra.Easy.Devel
    , module Numeric.LinearAlgebra.Easy.ST
    , module Numeric.LinearAlgebra.Easy.Types
    ) where

import           Numeric.LinearAlgebra.Easy.Basics
import           Numeric.LinearAlgebra.Easy.Devel
import           Numeric.LinearAlgebra.Easy.ST
import           Numeric.LinearAlgebra.Easy.Types
