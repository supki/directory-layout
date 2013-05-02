{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Wrappers around exception throwing functions and related routines
module System.Directory.Layout.Errored
  ( LayoutException(..)
  , createDirectory, createFile
  , fileExists, directoryExists, readFile
  , anyfail
  , relative
  ) where

import           Control.Exception hiding (try)
import qualified Control.Exception as E
import           Prelude hiding (readFile)
#if __GLASGOW_HASKELL__ >= 706
import           System.IO.Error
#else
import           System.IO.Error hiding (catch, try)
#endif

import           Control.Lens
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Monad.Writer.Class (MonadWriter, tell)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as D
import           System.FilePath (makeRelative)


try :: IO a -> IO (Either IOException a)
try = E.try

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
    CD IOErrorType FilePath      -- ^ 'createDirectory' exceptions
  | CF IOErrorType FilePath      -- ^ 'createFile' eceptions
  | FE IOErrorType FilePath      -- ^ 'fileExists' eceptions
  | DE IOErrorType FilePath      -- ^ 'directoryExists' eceptions
  | RF IOErrorType FilePath Text -- ^ 'readFile' eceptions
    deriving (Show, Eq)


-- | IO-exceptions-free 'D.createDirectory'
createDirectory :: MonadIO m => FilePath -> m (Either LayoutException ())
createDirectory fp = io $ try (D.createDirectory fp) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (CD (ioeGetErrorType e) fp)

-- | IO-exceptions-free 'T.writeFile'
createFile :: MonadIO m => FilePath -> Maybe Text -> m (Either LayoutException ())
createFile fp text = io $ try (createFileX fp text) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (CF (ioeGetErrorType e) fp)

createFileX :: FilePath -> Maybe Text -> IO ()
createFileX fp text = do
  x <- D.doesFileExist fp
  if x then
    ioError (mkIOError alreadyExistsErrorType "?" Nothing (Just fp))
  else
    T.writeFile fp (maybe T.empty id text)


-- | 'D.doesFileExist' that returns 'Either' instead of 'Bool'
fileExists :: MonadIO m => FilePath -> m (Either LayoutException ())
fileExists fp = io $ do
  p <- D.doesFileExist fp
  if p then
    return (Right ())
  else
    return (Left (FE doesNotExistErrorType fp))

-- | 'D.doesDirectoryExist' that returns 'Either' instead of 'Bool'
directoryExists :: MonadIO m => FilePath -> m (Either LayoutException ())
directoryExists fp = io $ do
  p <- D.doesDirectoryExist fp
  if p then
    return (Right ())
  else
    return (Left (DE doesNotExistErrorType fp))

-- | IO-exceptions-free 'T.readFile'
readFile :: MonadIO m => FilePath -> Text -> m (Either LayoutException ())
readFile fp text = io $ try (readFileX fp text) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (RF (ioeGetErrorType e) fp text)

readFileX :: FilePath -> Text -> IO ()
readFileX fp text = do
  text' <- T.readFile fp
  if text /= text' then
    ioError (mkIOError userErrorType "?" Nothing (Just fp))
  else
    return ()


-- | Make paths in 'LayoutException' relative to given 'FilePath'
relative :: FilePath -> LayoutException -> LayoutException
relative r (CD t fp)   = CD t (makeRelative r fp)
relative r (CF t fp)   = CF t (makeRelative r fp)
relative r (FE t fp)   = FE t (makeRelative r fp)
relative r (DE t fp)   = DE t (makeRelative r fp)
relative r (RF t fp c) = RF t (makeRelative r fp) c