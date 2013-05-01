{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module System.Directory.Layout.Make where

import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Text (Text)
import System.FilePath ((</>))

import System.Directory.Layout.Internal
import System.Directory.Layout.Errored


-- | Infect file layout with stuff from script
make ∷ Layout
     → FilePath             -- ^ Root directory
     → IO [LayoutException] -- ^ List of warnings
make z fp = map (relative fp) `fmap` runRunT fp (f z)
 where
  f (E _)           = return ()
  f (F p (E _) x)   = makeFile p Nothing >> f x
  f (F p (T t _) x) = makeFile p (Just t) >> f x
  f (D p x y)       = makeDirectory p >> changeDir p (f x) >> f y
  f _               = error "Broken DL () invariant"


type RunT = ReaderT FilePath (WriterT [LayoutException] IO)


runRunT ∷ FilePath → RunT a → IO [LayoutException]
runRunT e = execWriterT . flip runReaderT e


makeFile ∷ FilePath → Maybe Text -> RunT ()
makeFile p t = ask >>= \d -> anyfail $ createFile (d </> p) t


makeDirectory ∷ FilePath → RunT ()
makeDirectory p = ask >>= \d -> anyfail $ createDirectory (d </> p)


changeDir ∷ FilePath → RunT () → RunT ()
changeDir fp = local (</> fp)
