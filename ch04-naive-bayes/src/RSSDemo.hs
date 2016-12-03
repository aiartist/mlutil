module RSSDemo (runRSSDemos) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C8
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

runRSSDemos :: IO ()
runRSSDemos = do
    nyEntries <- rssEntries nyUrl
    sfEntries <- rssEntries sfUrl
    print $ head nyEntries
    print $ head sfEntries
