{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MLUtil.Roundable
    ( Roundable (..)
    ) where

import qualified Data.Vector.Storable as VS
import           MLUtil.Imports

class Roundable a where
    roundToPrecision :: Int -> a -> a

instance Roundable R where
    -- http://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
    roundToPrecision n x = (fromInteger $ round $ x * (10 ^ n)) / (10.0 ^^ n)

{-
instance (Roundable a, VS.Storable a) => Roundable (Vector a) where
    roundToPrecision n = VS.map (roundToPrecision n)

instance (Roundable a, VS.Storable a, Element a) => Roundable (Matrix a) where
    roundToPrecision n = mapMatrixWithIndex (\_ x -> roundToPrecision n x)
-}
instance Roundable Vector where
    roundToPrecision n = cmap (roundToPrecision n)

instance Roundable Matrix where
    roundToPrecision n = cmap (roundToPrecision n)

instance Roundable a => Roundable [a] where
    roundToPrecision n = map (roundToPrecision n)
