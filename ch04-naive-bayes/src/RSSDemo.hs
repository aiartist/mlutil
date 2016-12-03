module RSSDemo (runRSSDemos) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List
import qualified Data.Map as M
import           Data.Ord
import           MLUtil
import           Network.Wreq
import           Text.Feed.Import
import           Text.Feed.Query

nyUrl :: String
nyUrl = "http://newyork.craigslist.org/stp/index.rss"

sfUrl :: String
sfUrl = "http://sfbay.craigslist.org/stp/index.rss"

rssEntries :: String -> IO [String]
rssEntries url = do
    r <- get url
    let c = C8.unpack $ r ^. responseBody
        Just feed = parseFeedString c
    return $ map (\item -> let Just summary = getItemSummary item in summary) (feedItems feed)

-- cf bayes.localWords
localWords feedSummaries1 feedSummaries0 =
    let minLen = min (length feedSummaries1) (length feedSummaries0)
    in minLen

-- cf bayes.calcMostFreq
calcMostFreq :: [String] -> [String] -> [(String, Int)]
calcMostFreq vocabList fullText =
    let ps = M.toList $ itemCounts fullText
    in take 30 $ sortOn (Down . snd) ps

runRSSDemos :: IO ()
runRSSDemos = do
    {-
    nyEntries <- rssEntries nyUrl
    sfEntries <- rssEntries sfUrl
    print $ head nyEntries
    print $ head sfEntries
    -}
    print $ localWords ["one", "two"] ["three", "four", "five"]
