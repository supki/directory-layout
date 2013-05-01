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
module System.Directory.Layout.Check (check) where

import Prelude hiding (readFile)

import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Text (Text)
import System.FilePath ((</>))

import System.Directory.Layout.Internal
import System.Directory.Layout.Errored


-- | Check directory layout corresponds to specified one
check :: Layout
      -> FilePath             -- ^ Root directory
      -> IO [LayoutException] -- ^ List of failures
check z fp = map (relative fp) `fmap` runCheckT fp (f z)
 where
  f (E _)           = return ()
  f (F p (E _) x)   = checkFile p Nothing >> f x
  f (F p (T t _) x) = checkFile p (Just t) >> f x
  f (D p x y)       = checkDirectory p >> changeDir p (f x) >> f y
  f _               = error "Broken DL () invariant"


type CheckT = ReaderT FilePath (WriterT [LayoutException] IO)


runCheckT :: FilePath -> CheckT a -> IO [LayoutException]
runCheckT e = execWriterT . flip runReaderT e


checkFile :: FilePath -> Maybe Text -> CheckT ()
checkFile p t = ask >>= \d -> anyfail $ case t of
  Nothing -> fileExists (d </> p)
  Just t' -> readFile (d </> p) t'


checkDirectory :: FilePath -> CheckT ()
checkDirectory p = ask >>= \d -> anyfail $ directoryExists (d </> p)


changeDir :: FilePath -> CheckT () -> CheckT ()
changeDir fp = local (</> fp)
