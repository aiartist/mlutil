module MLUtil.Random
    ( choiceExtract
    , choiceExtractN
    ) where

import           Control.Monad
import           MLUtil.Util
import           System.Random

-- |Removes and returns random element from list
choiceExtract :: [a] -> IO (Maybe ([a], a))
choiceExtract [] = return Nothing
choiceExtract xs = randomRIO (0, length xs - 1) >>= return . (flip removeAt) xs

-- |Removes and returns n random elements from list
choiceExtractN :: Int -> [a] -> IO (Maybe ([a], [a]))
choiceExtractN 0 xs = return $ Just (xs, [])
choiceExtractN _ [] = return Nothing
choiceExtractN n xs
    | n < 0 || n >= length xs = return Nothing
    | otherwise = do
        x <- foldM (\(xs', ys) _ -> do
                Just (xs'', y) <- choiceExtract xs'
                return (xs'', y : ys))
                (xs, [])
                [0 .. n - 1]
        return $ Just x
