module EmailSpec
    ( main
    , spec
    ) where

import           Ch04NaiveBayes.Email
import           Test.Hspec

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

main :: IO ()
main = hspec spec
