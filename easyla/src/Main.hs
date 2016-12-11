module Main (main) where

import LA

readMatrix :: FilePath -> IO Matrix
readMatrix path = do
    let m = 5
        n = 5
    return $ withSTMatrix 9 m n $ \x -> do
        writeMatrix x 0 0 10.0
        writeMatrix x 1 1 20.0
        writeMatrix x 2 2 30.0

main :: IO ()
main = do
    let m = (2 >< 3) [0..]
        x = 3 |> [0..]
        y = m #> x
        x' = asColumn x
        y' = m <> x'
        y'' = x <# (3 >< 2) [0..]
        a = ones 2 :: Vector
    print y
    print y'
    print y''
    print a
    print $ (ones (3, 2) :: Matrix)
    print $ (zeroes (5, 7) :: Matrix)
    m <- readMatrix "foo.mat"
    print m
