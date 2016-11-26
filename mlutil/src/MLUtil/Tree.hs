module MLUtil.Tree
    ( Arrow (..)
    , ArrowLabel (..)
    , LeafLabel (..)
    , NodeLabel (..)
    , Tree (..)
    ) where

-- |A label for an arrow within a tree
class ArrowLabel a where
    alLabel :: a -> String

-- |A label for a leaf within a tree
class LeafLabel a where
    llLabel :: a -> String

-- |A label for a node within a tree
class NodeLabel a where
    nlLabel :: a -> String

-- |An arrow is a child tree plus a label
data Arrow a b c = A (Tree a b c) a deriving (Eq, Show)

-- |A tree consisting of arrow labels, leaf labels and node labels
data Tree a b c = Leaf b | Node c [Arrow a b c] deriving (Eq, Show)
