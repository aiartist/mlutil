module RSSDemo (runRSSDemos) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.Wreq
import           Text.Feed.Import
import           Text.Feed.Query

runRSSDemos :: IO ()
runRSSDemos = do
    r <- get "http://newyork.craigslist.org/stp/index.rss"
    let c = C8.unpack $ r ^. responseBody
        Just feed = parseFeedString c

    forM_ (take 1 $ feedItems feed) $ \item -> do
        let summary = getItemSummary item
        print summary
