module Ch04NaiveBayes.Util
    ( tokens
    , trainAndTest
    ) where

import           Ch04NaiveBayes.NaiveBayes
import           Ch04NaiveBayes.Vocabulary
import           Data.Char
import           Data.List.Split
import           Data.Ratio
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
-- |Naive Bayes model trained from extracted portion of data
trainAndTest :: ExtractIndices -> [String] -> [String] -> Ratio Int
trainAndTest testExtract class0Strs class1Strs =
    let class0Tokens = map tokens class0Strs
        class1Tokens = map tokens class1Strs
        classLists = classList Class0 class0Tokens ++ classList Class1 class1Tokens
        v = vocabulary (concat $ map fst classLists)
        Just (trainingData, testData) = extract testExtract classLists
        trainingMatrix = foldr (\(xs, c) vs -> (wordSetVec v xs, c) : vs) [] trainingData
        model = trainNB0 trainingMatrix
        errorCount = foldr
            (\(xs, c) n ->
                let wordVec = wordSetVec v xs
                    c' = classifyNB model wordVec
                in if c' == c then n else n + 1)
            0
            testData
    in errorCount % (length testData)

classList :: Classification -> [a] -> [(a, Classification)]
classList cls = map (flip (,) cls)
