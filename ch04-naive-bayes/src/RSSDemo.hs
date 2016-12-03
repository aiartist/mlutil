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
import           Debug.Trace
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
localWords :: [String] -> [String] -> IO ()
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
    Just (trainingSet, testSet) <- choiceExtractN 20 docList
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

-- cf bayes.calcMostFreq
calcMostFreq :: Int -> [String] -> [String] -> [(String, Int)]
calcMostFreq n vocabList fullText =
    let ps = M.toList $ itemCounts fullText
    in take n $ sortOn (Down . snd) ps

runRSSDemos :: IO ()
runRSSDemos = do
    nyEntries <- rssEntries nyUrl
    sfEntries <- rssEntries sfUrl
    localWords nyEntries sfEntries
