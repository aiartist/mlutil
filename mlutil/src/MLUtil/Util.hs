module MLUtil.Util
    ( deleteAt
    , forFold
    , forFoldM
    , removeAt
    ) where

import           Control.Monad

-- |Description of 'forFold'
--
-- Examples:
--
-- >>> forFold 0 [1, 2, 3, 4] (+)
-- 10
forFold :: Foldable t => b -> t a -> (a -> b -> b) -> b
forFold = flip . flip foldr

-- |Description of 'forFoldM'
--
-- Examples:
--
-- >>> forFoldM 0 [1, 2, 3, 4] (\acc x -> return $ acc + x)
-- 10
forFoldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
forFoldM = flip . flip foldM

-- |Tuple consisting of list with element at given element removed and the element itself
removeAt :: Int -> [a] -> Maybe ([a], a)
removeAt idx xs
    | idx < 0 || idx >= length xs = Nothing
    | otherwise =  let (b, e) = splitAt idx xs in Just (b ++ drop 1 e, head e)

-- |List with element at given element removed, special case of removeAt
deleteAt :: Int -> [a] -> Maybe [a]
deleteAt idx xs = removeAt idx xs >>= (return . fst)
