module MLUtil.Tree
    ( Arrow (..)
    , Tree (..)
    ) where

-- |An arrow is a child tree plus a label
data Arrow a l n = A (Tree a l n) a deriving (Eq, Show)

-- |A tree consisting of arrow labels, leaf labels and node labels
data Tree a l n = Leaf l | Node n [Arrow a l n] deriving (Eq, Show)
