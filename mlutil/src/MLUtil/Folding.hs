module MLUtil.Folding
    ( columnHead
    , foldColumn
    ) where

import           MLUtil.Imports

columnHead :: Matrix -> Int -> R
columnHead m c = m `atIndex` (0, c)

foldColumn :: (R -> b -> b) -> b -> Matrix -> Int -> b
foldColumn f acc m c = foldr (\r acc' -> f (m `atIndex` (r, c)) acc') acc [0..rows m - 1]
