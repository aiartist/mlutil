{-# LANGUAGE RecordWildCards #-}

module GradientAscentSpec
    ( main
    , spec
    ) where

import           Ch05LogisticRegression.GradientAscent
import           DataFiles
import qualified Data.Vector.Unboxed as VU
import           MLUtil
import           MLUtil.Test
import           Test.Hspec

spec :: Spec
spec = do
    describe "gradAscent" $ do
        it "computes correct weights" $ do
            Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
            let values = ones (rows lmValues) 1 ||| lmValues
                labels = col (map fromIntegral (VU.toList lmLabelIds))
                weights = toLists $ gradAscent 0.001 500 values labels
            length weights `shouldBe` 3
            length (weights !! 0) `shouldBe` 1
            length (weights !! 1) `shouldBe` 1
            length (weights !! 2) `shouldBe` 1
            concat weights `shouldRoundTo` [4.12414, 0.48007, -0.61685]

    describe "stocGradAscent0" $ do
        it "computes correct weights" $ do
            Just LabelledMatrix{..} <- getDataFileName "testSet.txt" >>= readLabelledMatrix
            let values = ones (rows lmValues) 1 ||| lmValues
                labels = col (map fromIntegral (VU.toList lmLabelIds))
                weights = toLists $ stocGradAscent0 0.01 values labels
            length weights `shouldBe` 3
            length (weights !! 0) `shouldBe` 1
            length (weights !! 1) `shouldBe` 1
            length (weights !! 2) `shouldBe` 1
            concat weights `shouldRoundTo` [1.01702,0.85914,-0.3658]

main :: IO ()
main = hspec spec
