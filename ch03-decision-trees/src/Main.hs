module Main (main) where

import           FishDemo
import           LensDemo

main :: IO ()
main = do
    runFishDemos
    runLensDemos
