module EmailDemo (runEmailDemos) where

import           Ch04NaiveBayes.Email
import           Control.Monad
import           Paths_ch04_naive_bayes
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict as IOS

getDataFileNames :: FilePath -> IO [FilePath]
getDataFileNames dir = do
    fullDir <- getDataFileName dir
    fileNames <- listDirectory fullDir
    return $ map (fullDir </>) fileNames

runEmailDemos :: IO ()
runEmailDemos = do
    spamFileNames <- getDataFileNames "email/spam"
    hamFileNames <- getDataFileNames "email/ham"
    allWords <- foldM (\ts p -> IOS.readFile p >>= \c -> return $ ts ++ tokens c) [] spamFileNames
    print allWords
