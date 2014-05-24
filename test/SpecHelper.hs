module SpecHelper where

import System.IO.Temp (withSystemTempDirectory)

temporary :: (FilePath -> IO a) -> IO a
temporary = withSystemTempDirectory "directory-layout-XXXXXX"
