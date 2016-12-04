module EmailDemo (runEmailDemos) where

import           Ch04NaiveBayes.Util
import           Data.List
import           DataFiles
import           MLUtil
import           System.Directory
import           System.FilePath

getDataFileNames :: FilePath -> IO [FilePath]
getDataFileNames dir = do
    fullDir <- getDataFileName dir
    fileNames <- sort <$> listDirectory fullDir
    return $ map (fullDir </>) fileNames

-- cf bayes.spamTest
spamTest :: IO ()
spamTest = do
    spamFileNames <- getDataFileNames "email/spam"
    spamFileStrs <- mapM readChar8File spamFileNames
    hamFileNames <- getDataFileNames "email/ham"
    hamFileStrs <- mapM readChar8File hamFileNames
    errorRate <- trainAndTest spamFileStrs hamFileStrs
    putStrLn $ "spamTest errorRate=" ++ show errorRate

runEmailDemos :: IO ()
runEmailDemos = spamTest
