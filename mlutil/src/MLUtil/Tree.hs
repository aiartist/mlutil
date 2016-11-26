module MLUtil.Tree
    ( Arrow (..)
    , ArrowLabel (..)
    , Class (..)
    , Label (..)
    , Tree (..)
    ) where

class ArrowLabel a where
    alLabel :: a -> String

-- TODO: Rename this to make it less specific to decision trees
newtype Class = C { unClass :: String } deriving (Eq, Ord, Show)

-- TODO: Rename this to make it less specific to decision trees
newtype Label = L { unLabel :: String } deriving (Eq, Show)

-- An arrow is a child tree plus a label
data Arrow a = A (Tree a) a deriving (Eq, Show)

data Tree a = Leaf Class | Node Label [Arrow a] deriving (Eq, Show)
