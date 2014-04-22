{-# LANGUAGE FlexibleContexts #-}
-- | Wrappers around exception throwing functions and related routines
module System.Directory.Layout.Errored
  ( LayoutException(..)
  , createDirectory, createFile, createLink
  , fileExists, directoryExists, readFile, checkSource
  , anyfail
  , relative
  ) where

import           Control.Exception
import           Control.Monad (when, unless)
import           Data.Maybe (fromMaybe)
import           Prelude hiding (readFile)
import           System.IO.Error
import           System.Posix.Files (createSymbolicLink, readSymbolicLink)

import           Control.Lens
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Monad.Writer.Class (MonadWriter, tell)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as D
import           System.FilePath (makeRelative)


io :: MonadIO m => IO a -> m a
io = liftIO


-- | Log failures
anyfail :: MonadWriter [w] m => m (Either w a) -> m ()
anyfail mewa = do
  ewa <- mewa
  case ewa of
    Left e -> tell [e]
    _      -> return ()


-- | Information about cought exceptions in various routines
data LayoutException =
    CD IOErrorType FilePath        -- ^ 'createDirectory' exceptions
  | CS IOErrorType FilePath String -- ^ 'checkSource' eceptions
  | CF IOErrorType FilePath        -- ^ 'createFile' eceptions
  | FE IOErrorType FilePath        -- ^ 'fileExists' eceptions
  | DE IOErrorType FilePath        -- ^ 'directoryExists' eceptions
  | RF IOErrorType FilePath Text   -- ^ 'readFile' eceptions
    deriving (Show, Eq)


-- | IO-exceptions-free 'D.createDirectory'
createDirectory :: MonadIO m => FilePath -> m (Either LayoutException ())
createDirectory fp = io $ tryIOError (D.createDirectory fp) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (CD (ioeGetErrorType e) fp)

-- | IO-exceptions-free 'T.writeFile'
createFile :: MonadIO m => FilePath -> Maybe Text -> m (Either LayoutException ())
createFile fp text = io $ tryIOError (createFileX fp text) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (CF (ioeGetErrorType e) fp)

-- | IO-exceptions-free 'createSymbolicLink'
createLink :: MonadIO m => FilePath -> String -> m (Either LayoutException ())
createLink fp s = io $ tryIOError (createSymbolicLink s fp) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (CF (ioeGetErrorType e) fp)

createFileX :: FilePath -> Maybe Text -> IO ()
createFileX fp text = do
  x <- D.doesFileExist fp
  if x then
    ioError (mkIOError alreadyExistsErrorType "?" Nothing (Just fp))
  else
    T.writeFile fp (fromMaybe T.empty text)


-- | 'D.doesFileExist' that returns 'Either' instead of 'Bool'
fileExists :: MonadIO m => FilePath -> m (Either LayoutException ())
fileExists fp = io $ do
  p <- D.doesFileExist fp
  return . unless p $
    Left (FE doesNotExistErrorType fp)

-- | 'D.doesDirectoryExist' that returns 'Either' instead of 'Bool'
directoryExists :: MonadIO m => FilePath -> m (Either LayoutException ())
directoryExists fp = io $ do
  p <- D.doesDirectoryExist fp
  return . unless p $
    Left (DE doesNotExistErrorType fp)

-- | IO-exceptions-free 'T.readFile'
readFile :: MonadIO m => FilePath -> Text -> m (Either LayoutException ())
readFile fp text = io $ tryIOError (readFileX fp text) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (RF (ioeGetErrorType e) fp text)

readFileX :: FilePath -> Text -> IO ()
readFileX fp text = do
  text' <- T.readFile fp
  when (text /= text') $
    ioError (mkIOError userErrorType "?" Nothing (Just fp))

checkSource :: MonadIO m => FilePath -> String -> m (Either LayoutException ())
checkSource fp s = liftIO $ do
  s' <- readSymbolicLink fp
  return . when (s /= s') $
    Left (CS userErrorType fp s)

-- | Make paths in 'LayoutException' relative to given 'FilePath'
relative :: FilePath -> LayoutException -> LayoutException
relative r (CD t fp)   = CD t (makeRelative r fp)
relative r (CS t fp s) = CS t (makeRelative r fp) s
relative r (CF t fp)   = CF t (makeRelative r fp)
relative r (FE t fp)   = FE t (makeRelative r fp)
relative r (DE t fp)   = DE t (makeRelative r fp)
relative r (RF t fp c) = RF t (makeRelative r fp) c
