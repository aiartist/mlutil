module NaiveBayesSpec
    ( main
    , spec
    ) where

import           Ch04NaiveBayes.NaiveBayes
import qualified Data.Set as S
import           Data.Vector ((//))
import qualified Data.Vector as V
import           Test.Hspec

-- cf bayes.loadDataSet
-- Class1 is abusive, Class0 not
dataSet :: [([String], Classification)]
dataSet =
    [ (["my", "dog", "has", "flea", "problems", "help", "please"], Class0)
    , (["maybe", "not", "take", "him", "to", "dog", "park", "stupid"], Class1)
    , (["my", "dalmatian", "is", "so", "cute", "I", "love", "him"], Class0)
    , (["stop", "posting", "stupid", "worthless", "garbage"], Class1)
    , (["mr", "licks", "ate", "my", "steak", "how", "to", "stop", "him"], Class0)
    , (["quit", "buying", "worthless", "dog", "food", "stupid"], Class1)
    ]

-- cf bayes.createVocabList
vocabulary :: [([String], Classification)] -> V.Vector String
vocabulary = V.fromList . S.toList . S.unions . map (S.fromList . fst)

-- cf bayes.setOfWords2Vec
wordVector :: V.Vector String -> [String] -> V.Vector Int
wordVector v ws = V.replicate (V.length v) 0 // foldr (\w ps -> let Just i = V.elemIndex w v in (i, 1) : ps) [] ws

spec :: Spec
spec = do
    describe "classifyNB" $ do
        let toVector = wordVector (vocabulary dataSet)
            model = trainNB0 (map (\(f, s) -> (toVector f, s)) dataSet)

        it "classifies non-abusive stuff" $
            classifyNB model (toVector ["love", "my", "dalmatian"]) `shouldBe` Class0

        it "classifies abusive stuff" $
            classifyNB model (toVector ["stupid", "garbage"]) `shouldBe` Class1

main :: IO ()
main = hspec spec
