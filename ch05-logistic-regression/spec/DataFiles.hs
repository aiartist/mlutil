module DataFiles (getDataFileName) where

import qualified Paths_ch05_logistic_regression as P

getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
