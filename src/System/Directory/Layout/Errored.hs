{-# LANGUAGE FlexibleContexts #-}
module System.Directory.Layout.Errored where

import           Control.Exception hiding (try)
import qualified Control.Exception as E
import           System.IO.Error

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


anyfail :: MonadWriter [w] m => m (Either w a) -> m ()
anyfail mewa = do
  ewa <- mewa
  case ewa of
    Left e -> tell [e]
    _      -> return ()


data LayoutException =
    CD IOErrorType FilePath
  | CF IOErrorType FilePath
  | FE IOErrorType FilePath
  | DE IOErrorType FilePath
  | RF IOErrorType FilePath Text
    deriving (Show, Eq)


createDirectory :: MonadIO m => FilePath -> m (Either LayoutException ())
createDirectory fp = io $ try (D.createDirectory fp) <&> \x -> case x of
  Right () -> Right ()
  Left  e  -> Left (CD (ioeGetErrorType e) fp)

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


fileExists :: MonadIO m => FilePath -> m (Either LayoutException ())
fileExists fp = io $ do
  p <- D.doesFileExist fp
  if p then
    return (Right ())
  else
    return (Left (FE doesNotExistErrorType fp))

directoryExists :: MonadIO m => FilePath -> m (Either LayoutException ())
directoryExists fp = io $ do
  p <- D.doesDirectoryExist fp
  if p then
    return (Right ())
  else
    return (Left (DE doesNotExistErrorType fp))

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


relative :: FilePath -> LayoutException -> LayoutException
relative r (CD t fp)   = CD t (makeRelative r fp)
relative r (CF t fp)   = CF t (makeRelative r fp)
relative r (FE t fp)   = FE t (makeRelative r fp)
relative r (DE t fp)   = DE t (makeRelative r fp)
relative r (RF t fp c) = RF t (makeRelative r fp) c
