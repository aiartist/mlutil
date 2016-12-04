module MLUtil.Arithmetic
    ( ones
    , sumColumns
    , sumRows
    ) where

import           MLUtil.Imports

ones :: Int -> Int -> Matrix R
ones m n = konst 1.0 (m, n)

sumColumns :: Matrix R -> Matrix R
sumColumns m = ones 1 (rows m) <> m

sumRows :: Matrix R -> Matrix R
sumRows m = ones 1 (cols m) <> tr m
