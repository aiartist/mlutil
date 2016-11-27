module EmailSpec
    ( main
    , spec
    ) where

import           Ch04NaiveBayes.Email
import           Test.Hspec

spec :: Spec
spec = do
    describe "tokens" $ do
        it "should tokenize simple sentence" $
            tokens "This book is the best book on Python or M.L. I have ever laid eyes upon." `shouldBe`
                ["this", "book", "is", "the", "best", "book", "on", "python"
                , "or", "m", "l", "i", "have", "ever", "laid", "eyes", "upon"]

        it "should collapse adjacent delimiters" $
            tokens "one  two  three  four" `shouldBe` ["one", "two", "three", "four"]

        it "should convert tokens to lower case" $
            tokens "ONE TWO THREE FOUR" `shouldBe` ["one", "two", "three", "four"]

        it "should split on punctuation" $
            tokens "one.two,three;four" `shouldBe` ["one", "two", "three", "four"]

        it "should split on symbols" $
            tokens "one=two" `shouldBe` ["one", "two"]

main :: IO ()
main = hspec spec
