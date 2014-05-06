module SpecHelper where

import System.Unix.Directory (withTemporaryDirectory)

temporary :: (FilePath -> IO a) -> IO a
temporary = withTemporaryDirectory "directory-layout-XXXXXX"
