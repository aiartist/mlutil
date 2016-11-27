module Ch04NaiveBayes.Email (tokens) where

import           Data.Char
import           Data.List.Split

tokens :: String -> [String]
tokens = map (map toLower) . wordsBy (\c -> isPunctuation c || isSpace c)
