module DataFiles (getDataFileName) where

import qualified Paths_ch06_svm as P

getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
