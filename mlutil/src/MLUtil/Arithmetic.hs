module MLUtil.Arithmetic
    ( sumColumns
    , sumRows
    ) where

import           MLUtil.Imports

-- TODO: Use <# and return Vector instead!
sumColumns :: Matrix -> Matrix
sumColumns m = ones (1, rows m) <> m

-- TODO: Use <# and return Vector instead!
sumRows :: Matrix -> Matrix
sumRows m = ones (1, cols m) <> tr' m
