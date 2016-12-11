module EasySpec
    ( main
    , spec
    ) where

import           Numeric.LinearAlgebra.Easy
import           Test.Hspec

spec :: Spec
spec = do
    describe "vector *" $
        it "performs elementwise product" $
            vector [1, 2, 3] * vector [3, 0, -2] `shouldBe` vector [3.0, 0.0, -6.0]

    describe "matrix *" $
        it "performs elementwise product" $
            matrix 3 [1..9] * ident 3 `shouldBe` (3 >< 3)
                                                    [ 1.0, 0.0, 0.0
                                                    , 0.0, 5.0, 0.0
                                                    , 0.0, 0.0, 9.0
                                                    ]

main :: IO ()
main = hspec spec
