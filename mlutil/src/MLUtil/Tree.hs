module MLUtil.Tree
    ( Arrow (..)
    , Class (..)
    , Label (..)
    , Tree (..)
    ) where

-- TODO: Rename this to make it less specific to decision trees
newtype Class = C { unClass :: String } deriving (Eq, Ord, Show)

-- TODO: Rename this to make it less specific to decision trees
newtype Label = L { unLabel :: String } deriving (Eq, Show)

data Arrow a = A (Tree a) String deriving (Eq, Show)

data Tree a = Leaf Class | Node Label [Arrow a] deriving (Eq, Show)
