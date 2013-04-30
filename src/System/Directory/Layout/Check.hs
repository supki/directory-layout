{-# LANGUAGE UnicodeSyntax #-}
-- | Check if current directory layout agrees with specified one
--
-- For example, suppose there is a tree:
--
-- @
-- % tree
-- .
-- ├── baz
-- │   └── twey
-- └── foo
--     ├── bar
--     │   ├── quuz
--     │   └── tatata
--     └── quux
-- @
--
-- then you can write:
--
-- @
-- layout = do
--   directory \"baz\" $
--     file_ \"twey\"
--   directory \"foo\" $ do
--     directory \"bar\" $ do
--       file_ \"quuz\"
--       file_ \"tatata\"
--     file_ \"quux\"
-- @
--
-- and running @check layout \".\"@ should result in @[]@
module System.Directory.Layout.Check
  ( DLCheckFailure(..), check
  ) where

import Control.Arrow (second)
import Control.Monad (unless, when)

import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.FilePath ((</>), makeRelative)
import           System.Directory

import System.Directory.Layout.Internal


-- | Check directory layout corresponds to specified one
check ∷ Layout
      → FilePath            -- ^ Root directory
      → IO [DLCheckFailure] -- ^ List of failures
check z fp = do
  d ← getCurrentDirectory
  fp' ← canonicalizePath fp
  setCurrentDirectory fp'
  xs ← runCheckT (fp', fp') (f z)
  setCurrentDirectory d
  return xs
 where
  f (E _) = return ()
  f (F p (E _) x) = fileExists p >> f x
  f (F p (T t _) x) = fileExists p >>= \w → when w (fileContains p t) >> f x
  f (D p x y) = dirExists p >>= \t → when t (changeDir p (f x)) >> f y
  f _ = error "Broken DL () invariant"


-- | Data type representing various failures
-- that may occur while checking directory layout
data DLCheckFailure =
    FileDoesNotExist FilePath
  | FileWrongContents FilePath Text
  | DirectoryDoesNotExist FilePath
    deriving (Show, Read, Eq, Ord)


type CheckT = ReaderT (FilePath, FilePath) (WriterT [DLCheckFailure] IO)


runCheckT ∷ (FilePath, FilePath) → CheckT a → IO [DLCheckFailure]
runCheckT e = execWriterT . flip runReaderT e


-- | File existence check
-- emits 'FileDoesNotExist' on failure
fileExists ∷ FilePath → CheckT Bool
fileExists p = do
  (r, d) ← ask
  z ← io $ doesFileExist (d </> p)
  unless z $
    tell' [FileDoesNotExist (makeRelative r d </> p)]
  return z


-- | Directory existence check
-- emits 'DirectoryDoesNotExist' on failure
dirExists ∷ FilePath → CheckT Bool
dirExists p = do
  (r, d) ← ask
  z ← io $ doesDirectoryExist (d </> p)
  unless z $
    tell' [DirectoryDoesNotExist (makeRelative r d </> p)]
  return z


-- | File contents check
-- emits 'FileDoesNotExist' on failure
fileContains ∷ FilePath → Text → CheckT ()
fileContains p c = do
  (r, d) ← ask
  z ← io $ T.readFile (d </> p)
  unless (z == c) $
    tell' [FileWrongContents (makeRelative r d </> p) z]


changeDir ∷ FilePath → CheckT () → CheckT ()
changeDir fp = local (second (</> fp))


io ∷ IO a → CheckT a
io = liftIO


tell' ∷ [DLCheckFailure] → CheckT ()
tell' = lift . tell
