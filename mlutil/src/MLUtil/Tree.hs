module MLUtil.Tree
    ( Branch (..)
    , Tree (..)
    ) where

-- |A branch is a child tree with a label
data Branch b l n = A (Tree b l n) b deriving (Eq, Show)

-- |A tree consisting of arrow labels, leaf labels and node labels
data Tree b l n = Leaf l | Node n [Branch b l n] deriving (Eq, Show)
