module Main (main) where

import           SigmoidDemo
import           UtilDemo

main :: IO ()
main = runUtilDemos >> runSigmoidDemos
