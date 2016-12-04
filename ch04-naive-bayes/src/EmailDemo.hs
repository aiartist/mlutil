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

genExtractIndices :: Int -> [a] -> [a] -> IO ExtractIndices
genExtractIndices c s h =
    let Just action = choiceExtractIndices c (length s + length h)
    in action >>= \ei -> return ei

-- cf bayes.spamTest
spamTest :: IO ()
spamTest = do
    spamFileNames <- getDataFileNames "email/spam"
    spamFileStrs <- mapM readChar8File spamFileNames
    hamFileNames <- getDataFileNames "email/ham"
    hamFileStrs <- mapM readChar8File hamFileNames
    ei <- genExtractIndices 1 spamFileStrs hamFileStrs
    let errorRate = trainAndTest ei spamFileStrs hamFileStrs
    putStrLn $ "spamTest error% = " ++ show (errorRate * 100.0)

runEmailDemos :: IO ()
runEmailDemos = spamTest
