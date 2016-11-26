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
class LeafLabel l where
    llLabel :: l -> String

-- |A label for a node within a tree
class NodeLabel n where
    nlLabel :: n -> String

-- |An arrow is a child tree plus a label
data Arrow a l n = A (Tree a l n) a deriving (Eq, Show)

-- |A tree consisting of arrow labels, leaf labels and node labels
data Tree a l n = Leaf l | Node n [Arrow a l n] deriving (Eq, Show)
