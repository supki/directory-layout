{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
-- | 'Layout' interpreters
module System.Directory.Layout.Interpreter
  ( pretty
  , spec
  , Validation(..)
  , fit
  , FitError(..)
  , make
  , MakeError(..)
  ) where

import           Control.Applicative
import           Control.Exception (Exception(..), SomeException(..), throwIO, try)
import           Control.Monad
import           Control.Monad.Free
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bitraversable (Bitraversable(..))
import qualified Data.ByteString as ByteString
import           Data.Data (Data, Typeable)
import           Data.Foldable (Foldable, sequenceA_, for_)
import           Data.Functor.Compose (Compose(..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup(..))
import qualified Data.Text.IO as Text
import           Data.Traversable (Traversable)
import           Data.Typeable (cast)
import           GHC.Generics (Generic)
import           Numeric (showOct)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (combine)
import           System.IO.Error (IOErrorType, ioeGetErrorType, ioeGetFileName, ioeGetLocation)
import qualified System.Posix as Posix
import           Test.Hspec (Spec, context, it)
import           Text.Printf (printf)

import           System.Directory.Layout.Internal

-- | Pretty print the directory layout
pretty :: Layout a -> String
pretty = unlines . iter go . unL . fmap (const []) where
  go f@(F _ _ _ other) = prettyF f : other
  go f@(SL _ _ _ _ other) = prettyF f : other
  go f@(D _ is _ other) = prettyF f : map indent is ++ other
  go E = []

  indent :: String -> String
  indent s = "┆ " ++ s

prettyF :: F a -> String
prettyF (F name cs _ _) = printf "‘%s’, %s" name (prettyC cs)
prettyF (SL name s _ _ _) = printf "‘%s’, a link to ‘%s’" name s
prettyF (D name _ _ _) = '/' : name
prettyF E = ""

prettyC :: Contents -> String
prettyC (B _) = "raw bytes"
prettyC (T _) = "text"
prettyC W = "whatever"

-- | Interpret the directory layout as a 'Spec'
spec :: FilePath -> Layout a -> Spec
spec p = go p . unL where
  go root (Free f@(F _ _ _ m)) = do
    specF root f
    go root m
  go root (Free f@(SL _ _ _ _ m)) = do
    specF root f
    go root m
  go root (Free f@(D (combine root -> fullpath) is _ m)) = do
    specF root f
    context (printf "directory ‘%s’" fullpath) (go fullpath is)
    go root m
  go _ (Free E) = return ()
  go _ (Pure _) = return ()

specF :: FilePath -> F a -> Spec
specF root = go where
  go f@(F name cs _ _) = it (printf "has a %s file ‘%s’" (examplesC cs) name) (fitIO root f)
  go f@(SL name s _ _ _) = it (printf "has a symlink ‘%s’ pointing to ‘%s’" name s) (fitIO root f)
  go f@(D (combine root -> fullpath) _ _ _) = it (printf "has a subdirectory ‘%s’" fullpath) (fitIO root f)
  go E = return ()

examplesC :: Contents -> String
examplesC (B _) = "binary"
examplesC (T _) = "plain text"
examplesC W = "regular"

validate
  :: Exception e
  => (forall a. FilePath -> F a -> IO ()) -> FilePath -> Layout b -> IO (Validation (NonEmpty e) ())
validate g p = getCompose . go p . unL where
  go root (Free f@(F _ _ _ m)) =
    sequenceA_ [Compose (validateF root f), go root m]
  go root (Free f@(SL _ _ _ _ m)) =
    sequenceA_ [Compose (validateF root f), go root m]
  go root (Free f@(D (combine root -> fullpath) is _ m)) =
    sequenceA_ [Compose (validateF root f), go fullpath is, go root m]
  go _ (Free E) = pure ()
  go _ (Pure _) = pure ()

  validateF root l = first pure . fromEither <$> try (g root l)

-- | Check the real directory layout fits the description
fit :: FilePath -> Layout a -> IO (Validation (NonEmpty FitError) ())
fit = validate fitIO

fitIO :: FilePath -> F a -> IO ()
fitIO root = go where
  go (F (combine root -> fullpath) cs a _) = do
    case cs of
      B bs -> do
        real <- ByteString.readFile fullpath
        when (real /= bs) $
          throwIO (FitBadFileContents fullpath cs (B real))
      T t -> do
        real <- Text.readFile fullpath
        when (real /= t) $
          throwIO (FitBadFileContents fullpath cs (T real))
      W -> return ()
    fitIOAux a fullpath
  go (SL (combine root -> fullpath) s e a _) = do
    path <- Posix.readSymbolicLink fullpath
    when (path /= s) $
      throwIO (FitBadLinkSource fullpath s path)
    when e $
      () <$ Posix.getFileStatus fullpath
    fitIOAux a fullpath
  go (D (combine root -> fullpath) _ a _) = () <$ do
    fitIOAux a fullpath
  go E = return ()

fitIOAux :: Aux -> FilePath -> IO ()
fitIOAux (Aux muid mgid mperm) path = do
  status <- Posix.getSymbolicLinkStatus path
  for_ muid $ \uid ->
    unless (Posix.fileOwner status == uid) $
      throwIO (FitBadOwnerUserID path uid (Posix.fileOwner status))
  for_ mgid $ \gid ->
    unless (Posix.fileGroup status == gid) $
      throwIO (FitBadOwnerGroupID path gid (Posix.fileGroup status))
  for_ mperm $ \perm ->
    unless (Posix.fileMode status == perm) $
      throwIO (FitBadFileMode path perm (Posix.fileMode status))

data FitError =
    FitBadFileContents FilePath Contents {- expected -} Contents {- actual -}
  | FitBadLinkSource FilePath String {- expected -} String {- actual -}
  | FitBadOwnerUserID FilePath Posix.UserID {- expected -} Posix.UserID {- actual -}
  | FitBadOwnerGroupID FilePath Posix.GroupID {- expected -} Posix.GroupID {- actual -}
  | FitBadFileMode FilePath Posix.FileMode {- expected -} Posix.FileMode {- actual -}
  | FitIOException FilePath IOErrorType
    deriving (Eq, Typeable, Generic)

instance Show FitError where
  show (FitBadFileContents path expected actual) = unlines $
    [ printf "Bad contents at ‘%s’" path
    , "expected:"
    , printf "  %s" (showC expected)
    , "actual:"
    , printf "  %s" (showC actual)
    ]
   where
    showC W = "whatever" -- that's not going to end up in the error messages anyway
    showC (B bs) = printf "%s" (show (ByteString.unpack bs))
    showC (T t) = printf "%s" (show t)
  show (FitBadLinkSource path expected actual) = unlines $
    [ printf "Bad symlink source at ‘%s’" path
    , "expected:"
    , printf "  ‘%s’" expected
    , "actual:"
    , printf "  ‘%s’" actual
    ]
  show (FitBadOwnerUserID path expected actual) = unlines $
    [ printf "Bad owner user id at ‘%s’" path
    , "expected:"
    , printf "  %s" (show expected)
    , "actual:"
    , printf "  %s" (show actual)
    ]
  show (FitBadOwnerGroupID path expected actual) = unlines $
    [ printf "Bad owner group id at ‘%s’" path
    , "expected:"
    , printf "  %s" (show expected)
    , "actual:"
    , printf "  %s" (show actual)
    ]
  show (FitBadFileMode path expected actual) = unlines $
    [ printf "Bad file permissions id at ‘%s’" path
    , "expected:"
    , printf "  %s" (showOct expected "")
    , "actual:"
    , printf "  %s" (showOct actual "")
    ]
  show (FitIOException eloc etype) =
    printf "Generic IO exception of type ‘%s’ happened at ‘%s’\n" (show etype) eloc

instance Exception FitError where
  toException = SomeException
  fromException e'@(SomeException e)
    | Just ioe <- fromException e' =
        Just (FitIOException (fromMaybe (ioeGetLocation ioe) (ioeGetFileName ioe)) (ioeGetErrorType ioe))
    | otherwise = cast e

-- | Make the real directory layout from the description
make :: FilePath -> Layout a -> IO (Validation (NonEmpty MakeError) ())
make = validate makeIO

makeIO :: FilePath -> F a -> IO ()
makeIO root = go where
  go (F (combine root -> fullpath) cs a _) = do
    case cs of
      B bs -> ByteString.writeFile fullpath bs
      T t -> Text.writeFile fullpath t
      W -> ByteString.writeFile fullpath (ByteString.pack [])
    makeIOAux a fullpath
  go (SL (combine root -> fullpath) s _ a _) = do
    Posix.createSymbolicLink s fullpath
    makeIOAux a fullpath
  go (D (combine root -> fullpath) _ a _) = do
    createDirectoryIfMissing False fullpath
    makeIOAux a fullpath
  go E = return ()

makeIOAux :: Aux -> FilePath -> IO ()
makeIOAux (Aux muid mgid mperm) path = do
  for_ muid $ \uid ->
    Posix.setOwnerAndGroup path uid (-1)
  for_ mgid $
    Posix.setOwnerAndGroup path (-1)
  for_ mperm $
    Posix.setFileMode path

data MakeError =
  MakeIOException FilePath IOErrorType
  deriving (Show, Eq, Typeable, Generic)

instance Exception MakeError where
  toException = SomeException
  fromException e'@(SomeException e)
    | Just ioe <- fromException e' =
        Just (MakeIOException (fromMaybe (ioeGetLocation ioe) (ioeGetFileName ioe)) (ioeGetErrorType ioe))
    | otherwise = cast e

data Validation e a = Error e | Result a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Typeable, Data, Generic)

instance Bifunctor Validation where
  bimap f _ (Error a) = Error (f a)
  bimap _ g (Result a) = Result (g a)

instance Bifoldable Validation where
  bifoldMap f _ (Error a) = f a
  bifoldMap _ g (Result a) = g a

instance Bitraversable Validation where
  bitraverse f _ (Error a) = Error <$> f a
  bitraverse _ g (Result a) = Result <$> g a

instance Semigroup e => Applicative (Validation e) where
  pure = Result
  Error f  <*> Error x  = Error (f <> x)
  Error f  <*> _        = Error f
  _        <*> Error x  = Error x
  Result f <*> Result x = Result (f x)

fromEither :: Either e a -> Validation e a
fromEither = either Error Result
