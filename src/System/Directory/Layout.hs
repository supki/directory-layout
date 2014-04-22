-- | Language to express directory layouts
module System.Directory.Layout
  ( -- * Layout declaration
    Node, Layout, file, file_, directory, directory_
    -- * Layout construction
  , fromDirectory
    -- * Layout traverses
  , make, check
    -- * Errors
  , LayoutException(..)
  ) where

import           Control.Lens
import           Control.Monad ((>=>))
import qualified Control.Exception as E
import           Data.Monoid (mconcat)
import           Data.Text (Text)
import qualified System.Directory as D
import           System.FilePath (combine)
import           System.FilePath.Lens (filename)

import System.Directory.Layout.Internal (Node(..), Layout)
import System.Directory.Layout.Traverse (make, check)
import System.Directory.Layout.Errored (LayoutException(..))


-- | Declare file with specified contents
file :: FilePath -> Text -> Layout
file x t = F x (Just t) (return ())
{-# INLINE file #-}


-- | Declare empty file
file_ :: FilePath -> Layout
file_ x = F x Nothing (return ())
{-# INLINE file_ #-}


-- | Declare directory with specified listing
directory :: FilePath -> Layout -> Layout
directory x d = D x d (return ())
{-# INLINE directory #-}


-- | Declare empty directory
directory_ :: FilePath -> Layout
directory_ x = D x (return ()) (return ())
{-# INLINE directory_ #-}


-- | Create layout from directory
--
-- Canonicalizes path before traversing, generally understands only
-- regular files and directories and ignores anything else it could not understand
fromDirectory :: FilePath -> IO (Either E.IOException Layout)
fromDirectory = E.try . (D.canonicalizePath >=> traverseDirectory)
 where
  traverseDirectory :: FilePath -> IO Layout
  traverseDirectory path = getDirectoryContents path >>=
    traverse (traverseFilePath . combine path) <&> directory (path^.filename) . mconcat

  traverseFilePath :: FilePath -> IO Layout
  traverseFilePath path = do
    isFile      <- D.doesFileExist path
    isDirectory <- D.doesDirectoryExist path
    case (isFile, isDirectory) of
      (True, _) -> return (file_ (path^.filename))
      (_, True) -> traverseDirectory path
      -- Should be pretty rare in practice: broken symlinks and stuff
      (_, _)    -> return (return ())

  getDirectoryContents :: FilePath -> IO [FilePath]
  getDirectoryContents = fmap (filter (not . (`elem` [".", ".."]))) . D.getDirectoryContents
