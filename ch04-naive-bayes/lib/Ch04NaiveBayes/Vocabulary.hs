module Ch04NaiveBayes.Vocabulary
    ( vocabulary
    , wordBagVec
    , wordSetVec
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Vector ((//))
import qualified Data.Vector as V

-- cf bayes.createVocabList
vocabulary :: [String] -> V.Vector String
vocabulary = V.fromList . S.toAscList . S.fromList

-- cf bayes.bagOfWords2VecMN
wordBagVec :: V.Vector String -> [String] -> V.Vector Int
wordBagVec v ws = V.replicate (V.length v) 0 // (M.toList $ foldr (\w m ->
    let Just i = w `V.elemIndex` v
    in M.alter (\mb -> case mb of Nothing -> Just 1; Just n -> Just $ n + 1) i m)
    M.empty
    ws)

-- cf bayes.setOfWords2Vec
wordSetVec :: V.Vector String -> [String] -> V.Vector Int
wordSetVec v ws = V.replicate (V.length v) 0 // foldr (\w ps ->
    let Just i = w `V.elemIndex` v in (i, 1) : ps)
    []
    ws
