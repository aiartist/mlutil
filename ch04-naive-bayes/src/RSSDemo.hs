{-# LANGUAGE RecordWildCards #-}

module RSSDemo (runRSSDemos) where

import           Ch04NaiveBayes.NaiveBayes
import           Ch04NaiveBayes.Util
import           Ch04NaiveBayes.Vocabulary
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List
import qualified Data.Map as M
import           Data.Ord
import qualified Data.Vector as V
import           MLUtil
import           Network.Wreq
import           Text.Feed.Import
import           Text.Feed.Query

nyUrl :: String
nyUrl = "http://newyork.craigslist.org/stp/index.rss"

nyClass :: Classification
nyClass = Class1

sfUrl :: String
sfUrl = "http://sfbay.craigslist.org/stp/index.rss"

sfClass :: Classification
sfClass = Class0

rssEntries :: String -> IO [String]
rssEntries url = do
    r <- get url
    let c = C8.unpack $ r ^. responseBody
        Just feed = parseFeedString c
    return $ map (\item -> let Just summary = getItemSummary item in summary) (feedItems feed)

classifiedList :: Classification -> [a] -> [(a, Classification)]
classifiedList cls = map (flip (,) cls)

-- cf bayes.localWords
localWords :: [String] -> [String] -> IO (V.Vector String, NaiveBayesModel)
localWords nySummaries sfSummaries = do
    let minLen = min (length nySummaries) (length sfSummaries)
        nyWordLists = take minLen $ map tokens nySummaries
        sfWordLists = take minLen $ map tokens sfSummaries
        docList = classifiedList nyClass nyWordLists ++ classifiedList sfClass sfWordLists
        fullText = concat [concat nyWordLists, concat sfWordLists]
        vocabV = vocabulary (concat $ map fst docList)
        vocabList = V.toList vocabV
        top30Words = calcMostFreq 30 vocabList fullText
        vocabList' =  V.fromList $ vocabList \\ map fst top30Words
    Just (trainingSet, testSet) <- choiceExtract 20 docList
    let trainMat = foldr (\(xs, c) vs -> (wordBagVec vocabList' xs, c) : vs) [] trainingSet
        model = trainNB0 trainMat
        errorCount = foldr
            (\(xs, c) n ->
                let wordVec = wordBagVec vocabList' xs
                    c' = classifyNB model wordVec
                in if c' == c then n else n + 1)
            0
            testSet
        errorRate = 100.0 * fromIntegral errorCount / fromIntegral (length testSet)
    print errorRate
    return (vocabList', model)

-- cf bayes.calcMostFreq
calcMostFreq :: Int -> [String] -> [String] -> [(String, Int)]
calcMostFreq n vocabList fullText =
    let ps = M.toList $ itemCounts fullText
    in take n $ sortOn (Down . snd) ps

-- cf bayes.getTopWords
getTopWords :: [String] -> [String] -> IO ()
getTopWords ny sf = do
    (vocabList, NaiveBayesModel{..}) <- localWords ny sf
    let (topNY, topSF) = V.foldr (\(x, p0, p1) (topNY, topSF) ->
                let topNY' = if p1 > -6.0 then (x, p1) : topNY else topNY
                    topSF' = if p0 > -6.0 then (x, p0) : topSF else topSF
                in (topNY', topSF'))
                ([], [])
                (V.zip3 vocabList nbmP0Vector nbmP1Vector)
        sortedNY = sortOn (Down . snd) topNY
        sortedSF = sortOn (Down . snd) topSF
    print sortedSF
    print sortedNY

runRSSDemos :: IO ()
runRSSDemos = do
    nyEntries <- rssEntries nyUrl
    sfEntries <- rssEntries sfUrl
    getTopWords nyEntries sfEntries
