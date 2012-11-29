{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Make layout as specified
--
-- For example, suppose you are in an empty directory
--
-- @
-- % tree
-- .
-- @
--
-- and you've written simple layout:
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
-- then running it should result in this directory tree:
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
module System.Directory.Layout.Make
  ( DLMakeWarning(..), make
  ) where

import Control.Arrow (second)

import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.FilePath ((</>), makeRelative)
import           System.Directory

import System.Directory.Layout.Internal


-- | Infect file layout with stuff from script
make ∷ Layout
     → FilePath          -- ^ Root directory
     → IO [DLMakeWarning] -- ^ List of warnings
make z fp = do
  d ← getCurrentDirectory
  fp' ← canonicalizePath fp
  setCurrentDirectory fp'
  xs ← runRunT (fp', fp') (f z)
  setCurrentDirectory d
  return xs
 where
  f (E _) = return ()
  f (F p Nothing x) = touchFile p >> f x
  f (F p (Just c) x) = touchFile p >> infectFile p c >> f x
  f (D p x y) = createDir p >> changeDir p (f x) >> f y


-- | Data type representing various warnings
-- that may occur while infecting directory layout
data DLMakeWarning =
    FileDoesExist FilePath
  | DirectoryDoesExist FilePath
    deriving (Show, Read, Eq, Ord)


type RunT = ReaderT (FilePath, FilePath) (WriterT [DLMakeWarning] IO)


runRunT ∷ (FilePath, FilePath) → RunT a → IO [DLMakeWarning]
runRunT e = execWriterT . flip runReaderT e


-- | File creation
-- emits 'FileDoesExist' if file exists already
touchFile ∷ FilePath → RunT ()
touchFile p = do
  (r, d) ← ask
  z ← io $ doesFileExist (d </> p)
  if z
    then tell' [FileDoesExist (makeRelative r d </> p)]
    else io $ T.writeFile (d </> p) ""


infectFile ∷ FilePath → Text → RunT ()
infectFile p c = do
  (_, d) ← ask
  io $ T.writeFile (d </> p) c


-- | Directory creation
-- emits 'DirectoryDoesExist' if directory exists already
createDir ∷ FilePath → RunT ()
createDir p = do
  (r, d) ← ask
  z ← io $ doesDirectoryExist (d </> p)
  if z
    then tell' [DirectoryDoesExist (makeRelative r d </> p)]
    else io $ createDirectory (d </> p)


changeDir ∷ FilePath → RunT () → RunT ()
changeDir fp = local (second (</> fp))


io ∷ IO a → RunT a
io = liftIO


tell' ∷ [DLMakeWarning] → RunT ()
tell' = lift . tell
