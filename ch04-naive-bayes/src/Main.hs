module Main (main) where

import qualified Data.Set as S
import           Data.Vector ((//))
import qualified Data.Vector as V
import           Prelude hiding (Word, sum)

type Word = String
type Sentence = [Word]
type Vocabulary = V.Vector Word

-- cf bayes.loadDataSet
sentences :: [Sentence]
sentences =
    [ ["my", "dog", "has", "flea", "problems", "help", "please"]
    , ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"]
    , ["my", "dalmation", "is", "so", "cute", "I", "love", "him"]
    , ["stop", "posting", "stupid", "worthless", "garbage"]
    , ["mr", "licks", "ate", "my", "steak", "how", "to", "stop", "him"]
    , ["quit", "buying", "worthless", "dog", "food", "stupid"]
    ]

-- cf bayes.loadDataSet
-- 1 is abusive, 0 not
classes :: [Int]
classes = [0, 1, 0, 1, 0, 1]

-- cf bayes.createVocabList
vocabulary :: [Sentence] -> Vocabulary
vocabulary = V.fromList . S.toList . S.unions . map S.fromList

-- cf bayes.setOfWords2Vec
wordVector :: Vocabulary -> Sentence -> V.Vector Int
wordVector v ws = V.replicate (V.length v) 0 // foldr (\w ps -> let Just i = V.elemIndex w v in (i, 1) : ps) [] ws

addV :: Num a => V.Vector a -> V.Vector a -> V.Vector a
addV = V.zipWith (+)

addScalar :: Num a => a -> V.Vector a -> V.Vector a
addScalar x = V.map (+ x)

sumV :: Num a => V.Vector a -> a
sumV = V.foldr1 (+)

divV :: (Integral a, Fractional b) => V.Vector a -> V.Vector a -> V.Vector b
divV xs ys = V.zipWith (\x y -> fromIntegral x / fromIntegral y) xs ys

logV :: Floating a => V.Vector a -> V.Vector a
logV = V.map log

-- cf bayes.trainNB0
trainNB0 :: [(V.Vector Int, Int)] -> (V.Vector Double, V.Vector Double, Double)
trainNB0 rows =
    let columnCount = (V.length . fst . head) rows
        initialNum = V.replicate columnCount 1
        initialDenom = V.replicate columnCount 2
        (rowCount, p0Num, p0Denom, p1Num, p1Denom, classTotal) = foldr (\(xs, c) (n, p0Num, p0Denom, p1Num, p1Denom, classTotal) ->
            if c == 1
                then (n + 1, p0Num, p0Denom, addV p1Num xs, addScalar (sumV xs) p1Denom, classTotal + c)
                else (n + 1, addV p0Num xs, addScalar (sumV xs) p0Denom, p1Num, p1Denom, classTotal + c)
            )
            (0, initialNum, initialDenom, initialNum, initialDenom, 0)
            rows
        p0Vector = logV $ divV p0Num p0Denom
        p1Vector = logV $ divV p1Num p1Denom
        pAbusive = fromIntegral classTotal / fromIntegral rowCount
    in (p0Vector, p1Vector, pAbusive)

main :: IO ()
main = do
    let v = vocabulary sentences
    print v
    print $ sentences !! 0
    print $ wordVector v (sentences !! 0)
    print $ sentences !! 3
    print $ wordVector v (sentences !! 3)
    let r = trainNB0 (zip (map (wordVector v) sentences) classes)
    print r
