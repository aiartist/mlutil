module EmailDemo (runEmailDemos) where

import           Ch04NaiveBayes.Email
import           Ch04NaiveBayes.NaiveBayes
import           Ch04NaiveBayes.Vocabulary
import           Control.Exception
import           Data.List
import           Paths_ch04_naive_bayes
import           System.Directory
import           System.FilePath
import qualified System.IO as IO
import qualified System.IO.Strict as IOS

readFileWithEncoding :: IO.TextEncoding -> FilePath -> IO String
readFileWithEncoding encoding path = bracket
    (IO.openFile path IO.ReadMode >>= \h -> IO.hSetEncoding h encoding >> return h)
    IO.hClose
    (IOS.hGetContents)

-- Just encode it: I don't care if we have to discard characters to do it!
readChar8File :: FilePath -> IO String
readChar8File = readFileWithEncoding IO.char8

getDataFileNames :: FilePath -> IO [FilePath]
getDataFileNames dir = do
    fullDir <- getDataFileName dir
    fileNames <- sort <$> listDirectory fullDir
    return $ map (fullDir </>) fileNames

classifiedList :: Classification -> [a] -> [(a, Classification)]
classifiedList cls = map (flip (,) cls)

runEmailDemos :: IO ()
runEmailDemos = do
    spamFileNames <- getDataFileNames "email/spam"
    spamFileStrs <- mapM readChar8File spamFileNames

    hamFileNames <- getDataFileNames "email/ham"
    hamFileStrs <- mapM readChar8File hamFileNames

    let spamWordLists = map tokens spamFileStrs
        hamWordLists = map tokens hamFileStrs
        docList = classifiedList Class1 spamWordLists ++
                    classifiedList Class0 hamWordLists
        fullText = concat [concat spamWordLists, concat hamWordLists]
        v = vocabulary (concat $ map fst docList)
    print $ length docList
    print $ length fullText
    print $ length v