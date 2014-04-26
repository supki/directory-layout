module SpecHelper where

import Data.List.NonEmpty (NonEmpty(..))
import System.Unix.Directory (withTemporaryDirectory)

import System.Directory.Layout.Interpreter (Validation(..))

temporary :: (FilePath -> IO a) -> IO a
temporary = withTemporaryDirectory "directory-layout-XXXXXX"

errors :: [e] -> Validation (NonEmpty e) ()
errors [] = Result ()
errors (x : xs) = Error (x :| xs)
