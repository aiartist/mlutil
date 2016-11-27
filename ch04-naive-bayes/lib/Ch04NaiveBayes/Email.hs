module Ch04NaiveBayes.Email (tokens) where

import           Data.Char
import           Data.List.Split

-- |Tokens in String
-- Unlike the Python code in Machine Learning in Action, this code purposefully
-- does not split on certain Unicode characters such as "right single quotation
-- mark" (U+2019) etc. This is not a big deal and shouldn't significant affect
-- the accuracy compared to that of the Python code.
--
-- We should consider using ICU transliteration etc. (see https://github.com/rcook/beginning-practical-haskell/blob/master/child-health-data/lib/ChildHealthData/CSV.hs)
-- to do a better job as necessary.
tokens :: String -> [String]
tokens = map (map toLower) . wordsBy (\c -> isPunctuation c || isSpace c || isSymbol c)
