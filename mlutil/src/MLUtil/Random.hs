module MLUtil.Random
    ( ExtractIndices (eiC, eiIs, eiN)
    , choiceExtract
    , choiceExtractIndices
    , choiceExtractIndices'
    , extract
    , isValidExtractIndices
    , mkExtractIndices
    ) where

import           Control.Monad
import           MLUtil.Util
import           System.Random

import Debug.Trace

data ExtractIndices = ExtractIndices
    { eiC :: Int
    , eiN :: Int
    , eiIs :: [Int]
    } deriving Show

-- |Make fixed c elements from n
mkExtractIndices :: Int -> [Int] -> Maybe ExtractIndices
mkExtractIndices n is =
    let ei = ExtractIndices (length is) n is
    in if isValidExtractIndices ei
        then Just ei
        else Nothing

-- |Is extract indices value valid or not
isValidExtractIndices :: ExtractIndices -> Bool
isValidExtractIndices ei = helper (eiC ei) (eiN ei) (eiIs ei)
    where
        helper c n is
            | length is /= c = False
            | otherwise = helper' n is
            where
                helper' n (i : is)
                    | i < 0 || i >= n = False
                    | otherwise = helper' (n - 1) is
                helper' n [] = True

-- |Extract elements
extract :: ExtractIndices -> [a] -> Maybe ([a], [a])
extract ei values = helper (eiN ei) (eiIs ei) values
    where
        helper n is values
            | length values /= n = Nothing
            | otherwise =
                let (residual', xs') = foldr
                        (\i (xs, ys) -> let Just (xs', x) = removeAt i xs in (xs', x : ys))
                        (values, [])
                        (reverse is)
                in Just (residual', reverse xs')

-- |Random choice of c elements from n
-- TODO: Rename to choiceExtractIndicesIO
choiceExtractIndices :: Int -> Int -> Maybe (IO ExtractIndices)
choiceExtractIndices c n
    | c < 0 || n < 0 || c > n = Nothing
    | c == 0 = (Just . return) $ ExtractIndices c n []
    | otherwise = Just $ do
        is <- foldM (\xs i -> randomRIO (0, i) >>= \x -> return $ x : xs) [] [0 .. c - 1]
        return $ ExtractIndices c n is

-- |Random choice of c elements from n
-- TODO: Rename to choiceExtractIndices
-- TODO: Consolidate with choiceExtractIndices
choiceExtractIndices' :: RandomGen g => g -> Int -> Int -> Maybe (ExtractIndices, g)
choiceExtractIndices' g c n
    | c < 0 || n < 0 || c > n = Nothing
    | c == 0 = Just (ExtractIndices c n [], g)
    | otherwise =
        let (is, g') = foldl
                (\(xs, g) i -> let (x, g') = randomR (0, i) g in (x : xs, g'))
                ([], g)
                [0 .. c - 1]
        in Just (ExtractIndices c n is, g')

-- |Removes and returns n random elements from list
choiceExtract :: Int -> [a] -> IO (Maybe ([a], [a]))
choiceExtract c values = do
    case choiceExtractIndices c (length values) of
        Nothing -> return Nothing
        Just action -> action >>= \ei -> return (extract ei values)
