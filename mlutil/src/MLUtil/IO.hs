module MLUtil.IO
    ( readChar8File
    , readFileWithEncoding
    ) where

import           Control.Exception
import qualified System.IO as IO
import qualified System.IO.Strict as IOS

-- |Read file with given encoding
readFileWithEncoding :: IO.TextEncoding -> FilePath -> IO String
readFileWithEncoding encoding path = bracket
    (IO.openFile path IO.ReadMode >>= \h -> IO.hSetEncoding h encoding >> return h)
    IO.hClose
    (IOS.hGetContents)

-- |Read file and force it to ASCII, regardless of data loss
readChar8File :: FilePath -> IO String
readChar8File = readFileWithEncoding IO.char8
