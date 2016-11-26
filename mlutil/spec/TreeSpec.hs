module TreeSpec
    ( main
    , spec
    ) where

import           Data.Binary
import           MLUtil
import           Test.Hspec

leaf :: String -> Tree a
leaf = Leaf . C

node :: String -> [Arrow a] -> Tree a
node = Node . L

spec :: Spec
spec = do
    describe "encode" $ do
        it "should roundtrip single leaf" $
            let tree = leaf "c0"
                bs = encode tree
                tree' = decode bs
            in tree' `shouldBe` tree

        it "should roundtrip tree" $
            let tree = node "l0"
                        [ A (leaf "c0") "f0"
                        , A (node "l1"
                                [ A (leaf "c1") "f1"
                                , A (leaf "c2") "f2"
                                ]) "f3"]
                bs = encode tree
                tree' = decode bs
            in tree' `shouldBe` tree

main :: IO ()
main = hspec spec
