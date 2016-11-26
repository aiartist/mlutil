{-# LANGUAGE DeriveGeneric #-}

module MLUtil.Tree
    ( Arrow (..)
    , Class (..)
    , Label (..)
    , Tree (..)
    ) where

import           Data.Binary
import           GHC.Generics (Generic)

-- TODO: Rename this to make it less specific to decision trees
newtype Class = C { unClass :: String } deriving (Eq, Generic, Ord, Show)
instance Binary Class

-- TODO: Rename this to make it less specific to decision trees
newtype Label = L { unLabel :: String } deriving (Eq, Generic, Show)
instance Binary Label

data Arrow a = A (Tree a) String deriving (Eq, Generic, Show)
instance Binary (Arrow a)

data Tree a = Leaf Class | Node Label [Arrow a] deriving (Eq, Generic, Show)
instance Binary (Tree a)
