module VocabularySpec
    ( main
    , spec
    ) where

import           Ch04NaiveBayes.Vocabulary
import           Data.Vector ((//))
import qualified Data.Vector as V
import           Test.Hspec

spec :: Spec
spec = do
    let dataSet =
            [ ["my", "dog", "has", "flea", "problems", "help", "please"]
            , ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"]
            , ["my", "dalmatian", "is", "so", "cute", "I", "love", "him"]
            , ["stop", "posting", "stupid", "worthless", "garbage"]
            , ["mr", "licks", "ate", "my", "steak", "how", "to", "stop", "him"]
            , ["quit", "buying", "worthless", "dog", "food", "stupid"]
            ]
        v = vocabulary (concat dataSet)

    describe "vocabulary" $
        it "returns flattened vector of unique words" $
             v `shouldBe` V.fromList
                [ "I", "ate", "buying", "cute", "dalmatian", "dog", "flea"
                , "food", "garbage", "has", "help", "him", "how", "is", "licks"
                , "love", "maybe", "mr", "my", "not", "park", "please"
                , "posting", "problems", "quit", "so", "steak", "stop"
                , "stupid", "take", "to", "worthless"
                ]

    describe "wordBagVec" $
        it "returns ordered count of words" $
            wordBagVec v ["buying", "cute", "buying", "dalmatian", "dog", "cute", "cute"] `shouldBe`
                V.replicate 32 0 // [(2, 2), (3, 3), (4, 1), (5, 1)]

    describe "wordSetVec" $
        it "returns ordered zeroes or ones" $
            wordSetVec v ["buying", "cute", "buying", "dalmatian", "dog", "cute", "cute"] `shouldBe`
                V.replicate 32 0 // [(2, 1), (3, 1), (4, 1), (5, 1)]

main :: IO ()
main = hspec spec
