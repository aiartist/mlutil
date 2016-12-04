module Main (main) where

import           GradientAscentDemo
import           UtilDemo

main :: IO ()
main = runUtilDemos >> runGradientAscentDemos
