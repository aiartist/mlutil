module DataFiles (getDataFileName) where

import qualified Paths_ch04_naive_bayes as P

getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
