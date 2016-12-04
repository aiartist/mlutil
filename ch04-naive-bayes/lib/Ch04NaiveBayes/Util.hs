module Ch04NaiveBayes.Util
    ( tokens
    , trainAndTest
    ) where

import           Ch04NaiveBayes.NaiveBayes
import           Ch04NaiveBayes.Vocabulary
import           Data.Char
import           Data.List.Split
import           MLUtil

-- cf bayes.textParse
-- |Tokens in String
-- Unlike the Python code in Machine Learning in Action, this code purposefully
-- does not split on certain Unicode characters such as "right single quotation
-- mark" (U+2019) etc. This is not a big deal and shouldn't significant affect
-- the accuracy compared to that of the Python code.
--
-- We should consider using ICU transliteration etc. (see https://github.com/rcook/beginning-practical-haskell/blob/master/child-health-data/lib/ChildHealthData/CSV.hs)
-- to do a better job as necessary.
tokens :: String -> [String]
tokens s = filter
            ((> 2) . length)
            (map (map toLower) . wordsBy (\c -> isPunctuation c || isSpace c || isSymbol c) $ s)

-- cf bayes.spamTest
-- |Return naive Bayes model trained from random partition of data
trainAndTest :: [String] -> [String] -> IO Double
trainAndTest spamFileStrs hamFileStrs = do
    let spamWordLists = map tokens spamFileStrs
        hamWordLists = map tokens hamFileStrs
        docList = classifiedList Class1 spamWordLists ++
                    classifiedList Class0 hamWordLists
        fullText = concat [concat spamWordLists, concat hamWordLists]
        vocabList = vocabulary (concat $ map fst docList)
    Just (trainingSet, testSet) <- choiceExtractN 10 docList
    let trainMat = foldr (\(xs, c) vs -> (wordSetVec vocabList xs, c) : vs) [] trainingSet
        model = trainNB0 trainMat
        errorCount = foldr
            (\(xs, c) n ->
                let wordVec = wordSetVec vocabList xs
                    c' = classifyNB model wordVec
                in if c' == c then n else n + 1)
            0
            testSet
        errorRate = 100.0 * fromIntegral errorCount / fromIntegral (length testSet)
    return errorRate

classifiedList :: Classification -> [a] -> [(a, Classification)]
classifiedList cls = map (flip (,) cls)
