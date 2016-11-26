module MLUtil.Tree
    ( Arrow (..)
    , ArrowLabel (..)
    , Label (..)
    , LeafLabel (..)
    , Tree (..)
    ) where

class ArrowLabel a where
    alLabel :: a -> String

class LeafLabel a where
    llLabel :: a -> String

-- TODO: Rename this to make it less specific to decision trees
newtype Label = L { unLabel :: String } deriving (Eq, Show)

-- An arrow is a child tree plus a label
data Arrow a b = A (Tree a b) a deriving (Eq, Show)

-- a is the arrow type
-- b is the leaf type
data Tree a b = Leaf b | Node Label [Arrow a b] deriving (Eq, Show)
