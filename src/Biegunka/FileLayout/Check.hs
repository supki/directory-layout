{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.FileLayout.Check
  ( FLCheckFailure(..), check
  ) where

import Control.Exception (SomeException, handle)
import Control.Monad (unless, when)

import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.FilePath ((</>), makeRelative)
import           System.Directory

import Biegunka.FileLayout.Internal (FL(..))


check ∷ FilePath → FL a → IO [FLCheckFailure]
check fp z = do
  d ← getCurrentDirectory
  fp' ← canonicalizePath fp
  setCurrentDirectory fp'
  xs ← runCheckT (fp', fp') (f z)
  setCurrentDirectory d
  return xs
 where
  f ∷ FL a → CheckT ()
  f (E _) = return ()
  f (F p Nothing x) = fileExists p >> f x
  f (F p (Just c) x) = fileExists p >>= \t → when t (fileContains p c) >> f x
  f (D p x y) = dirExists p >>= \t → when t (changeDir p (f x)) >> f y


data FLCheckFailure =
    FileDoesNotExist FilePath
  | FileWrongContents FilePath Text
  | DirectoryDoesNotExist FilePath
  | ChangeDirectoryFailed FilePath FilePath
    deriving (Show, Read, Eq, Ord)


type CheckT = ReaderT (FilePath, FilePath) (WriterT [FLCheckFailure] IO)


runCheckT ∷ (FilePath, FilePath) → CheckT a → IO [FLCheckFailure]
runCheckT e = execWriterT . flip runReaderT e


fileExists ∷ FilePath → CheckT Bool
fileExists p = do
  (r, d) ← ask
  z ← io $ doesFileExist p
  unless z $
    tell' [FileDoesNotExist (makeRelative r d </> p)]
  return z


dirExists ∷ FilePath → CheckT Bool
dirExists p = do
  (r, d) ← ask
  z ← io $ doesDirectoryExist p
  unless z $
    tell' [DirectoryDoesNotExist (makeRelative r d </> p)]
  return z


fileContains ∷ FilePath → Text → CheckT ()
fileContains p c = do
  (r, d) ← ask
  y ← io $ T.readFile p
  unless (y == c) $
    tell' [FileWrongContents (makeRelative r d </> p) y]


changeDir ∷ FilePath → CheckT () → CheckT ()
changeDir fp x = do
  (r, d) ← ask
  tell' `whenJust` (maybeHandle (setCurrentDirectory d) . runCheckT (r, d </> fp) $ cd fp >> x >> cd d)
 where
  cd = io . setCurrentDirectory


whenJust ∷ Monad m ⇒ (a → m b) → m (Maybe a) → m ()
whenJust f x = do
  p ← x
  case p of
    Just y → f y >> return ()
    Nothing → return ()


maybeHandle ∷ IO a → IO b → CheckT (Maybe b)
maybeHandle m = io . handle (\(_ ∷ SomeException) → m >> return Nothing) . fmap Just


io ∷ IO a → CheckT a
io = liftIO


tell' ∷ [FLCheckFailure] → CheckT ()
tell' = lift . tell
