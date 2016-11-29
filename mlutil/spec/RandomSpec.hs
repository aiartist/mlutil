module RandomSpec
    ( main
    , spec
    ) where

import           MLUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "dummy" $ do
        it "passes" $
            1 `shouldBe` 1

main :: IO ()
main = hspec spec
