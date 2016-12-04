module UtilSpec
    ( main
    , spec
    ) where

import           Ch04NaiveBayes.Util
import           Data.List
import           Data.Ratio
import           DataFiles
import           MLUtil
import           System.Directory
import           System.FilePath
import           Test.Hspec

getDataFileNames :: FilePath -> IO [FilePath]
getDataFileNames dir = do
    fullDir <- getDataFileName dir
    fileNames <- sort <$> listDirectory fullDir
    return $ map (fullDir </>) fileNames

getSpamAndHam :: IO ([String], [String])
getSpamAndHam = do
    spamFileNames <- getDataFileNames "email/spam"
    spam <- mapM readChar8File spamFileNames
    hamFileNames <- getDataFileNames "email/ham"
    ham <- mapM readChar8File hamFileNames
    return (spam, ham)

spec :: Spec
spec = do
    describe "tokens" $ do
        it "should tokenize simple sentence and remove small words" $
            tokens "This book is the best book on Python or M.L. I have ever laid eyes upon." `shouldBe`
                ["this", "book", "the", "best", "book", "python", "have"
                , "ever", "laid", "eyes", "upon"]

        it "should collapse adjacent delimiters" $
            tokens "one  two  three  four" `shouldBe` ["one", "two", "three", "four"]

        it "should convert tokens to lower case" $
            tokens "ONE TWO THREE FOUR" `shouldBe` ["one", "two", "three", "four"]

        it "should split on punctuation" $
            tokens "one.two,three;four" `shouldBe` ["one", "two", "three", "four"]

        it "should split on symbols" $
            tokens "one=two" `shouldBe` ["one", "two"]

        {-
        it "should transliterate Unicode" $ do
            tokens "you’re" `shouldBe` ["you", "re"]
            tokens "you\u2019re" `shouldBe` ["you", "re"]
        -}

    describe "trainAndTest" $ do
        it "should have 0 error rate" $ do
            (spam, ham) <- getSpamAndHam
            let Just testExtract = mkExtractIndices 50 [0] -- 49 elements of training data
            trainAndTest testExtract spam ham `shouldBe` 0

        it "should have medium error rate" $ do
            (spam, ham) <- getSpamAndHam
            let Just testExtract = mkExtractIndices 50 [34, 33 .. 1] -- 16 elements of training data
            trainAndTest testExtract spam ham `shouldBe` 5 % 17

        it "should have high error rate" $ do
            (spam, ham) <- getSpamAndHam
            let Just testExtract = mkExtractIndices 50 [49, 48 .. 1] -- 1 element of training data
            trainAndTest testExtract spam ham `shouldBe` 25 % 49

main :: IO ()
main = hspec spec
