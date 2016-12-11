module EasySpec
    ( main
    , spec
    ) where

import           Numeric.LinearAlgebra.Easy
import           Test.Hspec

spec :: Spec
spec = do
    describe "vector +" $
        it "performs elementwise add" $
            vector [1, 2, 3] + vector [3, 0, -2] `shouldBe` vector [4.0, 2.0, 1.0]

    describe "vector *" $
        it "performs elementwise product" $
            vector [1, 2, 3] * vector [3, 0, -2] `shouldBe` vector [3.0, 0.0, -6.0]

    describe "matrix +" $
        it "performs elementwise add" $
            matrix 3 [1..9] + ident 3 `shouldBe` (3 >< 3)
                                                    [ 2.0, 2.0, 3.0
                                                    , 4.0, 6.0, 6.0
                                                    , 7.0, 8.0, 10.0
                                                    ]

    describe "matrix *" $
        it "performs elementwise product" $
            matrix 3 [1..9] * ident 3 `shouldBe` (3 >< 3)
                                                    [ 1.0, 0.0, 0.0
                                                    , 0.0, 5.0, 0.0
                                                    , 0.0, 0.0, 9.0
                                                    ]

main :: IO ()
main = hspec spec
