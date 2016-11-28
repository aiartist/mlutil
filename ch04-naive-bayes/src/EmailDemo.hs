module EmailDemo (runEmailDemos) where

import           Ch04NaiveBayes.Email
import           Control.Monad
import           Data.List
import           Paths_ch04_naive_bayes
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict as IOS

getDataFileNames :: FilePath -> IO [FilePath]
getDataFileNames dir = do
    fullDir <- getDataFileName dir
    fileNames <- sort <$> listDirectory fullDir
    return $ map (fullDir </>) fileNames

type Doc = ([String], Int)

spamClass :: Int
spamClass = 1

hamClass :: Int
hamClass = 0

runEmailDemos :: IO ()
runEmailDemos = do
    spamFileNames <- getDataFileNames "email/spam"
    spamFileStrs <- mapM IOS.readFile spamFileNames

    hamFileNames <- getDataFileNames "email/ham"
    hamFileStrs <- mapM IOS.readFile hamFileNames

    let spamWordLists :: [[String]]
        spamWordLists = map tokens spamFileStrs
        hamWordLists :: [[String]]
        hamWordLists = map tokens hamFileStrs
        docList :: [([String], Int)]
        docList = map (flip (,) spamClass) spamWordLists ++ map (flip (,) hamClass) hamWordLists
        fullText :: [String]
        fullText = concat [concat spamWordLists, concat hamWordLists]
    print $ length docList
    print $ length fullText
