-- | 'Layout' traverses
module System.Directory.Layout.Traverse
  ( make, check
  ) where

import Prelude hiding (readFile)

import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Text (Text)
import System.FilePath ((</>))

import System.Directory.Layout.Internal
import System.Directory.Layout.Errored


type RunT = ReaderT FilePath (WriterT [LayoutException] IO)

runRunT :: FilePath -> RunT a -> IO [LayoutException]
runRunT e = execWriterT . flip runReaderT e

applyTraverse :: (Layout -> RunT ()) -> Layout -> FilePath -> IO [LayoutException]
applyTraverse f z fp = map (relative fp) `fmap` runRunT fp (f z)

changeDir :: FilePath -> RunT () -> RunT ()
changeDir fp = local (</> fp)


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
-- then running
--
-- @
-- make layout
-- @
--
-- should result in this directory structure:
--
-- @
-- % tree
-- .
-- ├── baz
-- │   └── twey
-- └── foo
--     ├── bar
--     │   ├── quuz
--     │   └── tatata
--     └── quux
-- @
--
make :: Layout
     -> IO [LayoutException] -- ^ List of warnings
make l = applyTraverse go l "."
 where
  go (E _)           = return ()
  go (F p (E _) x)   = makeFile p Nothing >> go x
  go (F p (T t _) x) = makeFile p (Just t) >> go x
  go (D p x y)       = makeDirectory p >> changeDir p (go x) >> go y
  go _               = error "Broken DL () invariant"

makeFile :: FilePath -> Maybe Text -> RunT ()
makeFile p t = ask >>= \d -> anyfail $ createFile (d </> p) t

makeDirectory :: FilePath -> RunT ()
makeDirectory p = ask >>= \d -> anyfail $ createDirectory (d </> p)


-- | Check directory layout agrees with specified one
--
-- For example, suppose there is a tree:
--
-- @
-- % tree
-- .
-- ├── baz
-- │   └── twey
-- └── foo
--     ├── bar
--     │   ├── quuz
--     │   └── tatata
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
check :: Layout
      -> FilePath             -- ^ Root directory
      -> IO [LayoutException] -- ^ List of failures
check = applyTraverse go
 where
  go :: Layout -> RunT ()
  go (E _)           = return ()
  go (F p (E _) x)   = checkFile p Nothing >> go x
  go (F p (T t _) x) = checkFile p (Just t) >> go x
  go (D p x y)       = checkDirectory p >> changeDir p (go x) >> go y
  go _               = error "Broken DL () invariant"

checkFile :: FilePath -> Maybe Text -> RunT ()
checkFile p t = ask >>= \d -> anyfail $ case t of
  Nothing -> fileExists (d </> p)
  Just t' -> readFile (d </> p) t'

checkDirectory :: FilePath -> RunT ()
checkDirectory p = ask >>= \d -> anyfail $ directoryExists (d </> p)
