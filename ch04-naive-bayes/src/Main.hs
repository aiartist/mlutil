module Main (main) where

import           Ch04NaiveBayes.NaiveBayes
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
    , ["my", "dalmatian", "is", "so", "cute", "I", "love", "him"]
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
wordVector :: Vocabulary -> Sentence -> WordVector
wordVector v ws = V.replicate (V.length v) 0 // foldr (\w ps -> let Just i = V.elemIndex w v in (i, 1) : ps) [] ws

main :: IO ()
main = do
    let v = vocabulary sentences
        toVector = wordVector v
        trainingMatrix = (map toVector sentences)
        model = trainNB0 (zip trainingMatrix classes)
    print $ classifyNB model (toVector ["love", "my", "dalmatian"])
    print $ classifyNB model (toVector ["stupid", "garbage"])
