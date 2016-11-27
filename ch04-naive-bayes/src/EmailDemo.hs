module EmailDemo (runEmailDemos) where

import           Ch04NaiveBayes.Email
import           Paths_ch04_naive_bayes
import qualified System.IO.Strict as IOS

runEmailDemos :: IO ()
runEmailDemos = do
    path <- getDataFileName "email/ham/6.txt"
    s <- IOS.readFile path
    --let s = "This book is the best book on Python or M.L. I have ever laid eyes upon."
    print $ tokens s
