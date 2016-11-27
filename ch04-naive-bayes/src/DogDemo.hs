module DogDemo (runDogDemos) where

import           Ch04NaiveBayes.NaiveBayes
import           Ch04NaiveBayes.Vocabulary

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

runDogDemos :: IO ()
runDogDemos = do
    let toSetVec = wordSetVec (vocabulary (concat $ map fst dataSet))
        model = trainNB0 (map (\(f, s) -> (toSetVec f, s)) dataSet)
    print $ classifyNB model (toSetVec ["love", "my", "dalmatian"])
    print $ classifyNB model (toSetVec ["stupid", "garbage"])
