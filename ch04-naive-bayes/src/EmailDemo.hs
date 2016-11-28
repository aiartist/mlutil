module EmailDemo (runEmailDemos) where

import           Ch04NaiveBayes.Email
import           Ch04NaiveBayes.NaiveBayes
import           Ch04NaiveBayes.Vocabulary
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
        docList :: [([String], Classification)]
        docList = map (flip (,) Class1) spamWordLists ++ map (flip (,) Class0) hamWordLists
        fullText :: [String]
        fullText = concat [concat spamWordLists, concat hamWordLists]
        v = vocabulary (concat $ map fst docList)
    print $ length docList
    print $ length fullText
    print $ length v
