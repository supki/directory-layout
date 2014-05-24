{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
-- | A bunch of 'Layout' description interpreters
module System.Directory.Layout.Interpreter
  ( pretty
  , spec
  , Validation(..)
  , fromErrors
  , fit
  , FitError(..)
  , FitContentsError(..)
  , make
  , remake
  , MakeError(..)
  ) where

import           Control.Applicative
import           Control.Exception (Exception(..), SomeException(..), throwIO, try)
import           Control.Monad
import           Control.Monad.Free
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bitraversable (Bitraversable(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import           Data.Data (Data, Typeable)
import           Data.Foldable (Foldable, sequenceA_, for_, toList)
import           Data.Functor.Compose (Compose(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup(..))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Data.Traversable (Traversable)
import           Data.Typeable (cast)
import           GHC.Generics (Generic)
import           Numeric (showOct)
import           System.Directory (createDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
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

prettyC :: Maybe Contents -> String
prettyC (Just (Binary _)) = "raw bytes"
prettyC (Just (Text _)) = "text"
prettyC (Just (CopyOf p)) = printf "(copy of ‘%s’)" p
prettyC Nothing = "anything"

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

examplesC :: Maybe Contents -> String
examplesC (Just (Binary _)) = "binary"
examplesC (Just (Text _)) = "plain text"
examplesC (Just (CopyOf p)) = printf "(copy of ‘%s’)" p
examplesC Nothing = "regular"

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

  validateF root = validateIO . g root

validateIO :: Exception e => IO a -> IO (Validation (NonEmpty e) a)
validateIO io = first pure . fromEither <$> try io

-- | Check the real directory layout fits the description
fit :: FilePath -> Layout a -> IO (Validation (NonEmpty FitError) ())
fit = validate fitIO

fitIO :: FilePath -> F a -> IO ()
fitIO root = go where
  go (F (combine root -> fullpath) cs a _) = do
    for_ cs $ \cs' -> case cs' of
      Binary bs -> do
        real <- ByteString.readFile fullpath
        when (real /= bs) $
          throwIO (FitBadFileContents fullpath (FitBadBinary bs real))
      Text t -> do
        real <- Text.readFile fullpath
        when (real /= t) $
          throwIO (FitBadFileContents fullpath (FitBadText t real))
      CopyOf f -> do
        origin <- ByteStringLazy.readFile f
        copy <- ByteStringLazy.readFile fullpath
        when (origin /= copy) $
          throwIO (FitBadFileContents fullpath (FitBadCopyOf f))
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
  for_ muid $ \case
    UserID i ->
      unless (Posix.fileOwner status == i) $
        throwIO (FitBadOwnerUser path (UserID i) (UserID (Posix.fileOwner status)))
    Username name -> do
      i <- getUserID name
      n <- getUsername (Posix.fileOwner status)
      unless (Posix.fileOwner status == i) $
        throwIO (FitBadOwnerUser path (Username name) (Username n))
  for_ mgid $ \case
    GroupID i ->
      unless (Posix.fileGroup status == i) $
        throwIO (FitBadOwnerGroup path (GroupID i) (GroupID (Posix.fileGroup status)))
    Groupname name -> do
      i <- getGroupID name
      n <- getGroupname (Posix.fileGroup status)
      unless (Posix.fileGroup status == i) $
        throwIO (FitBadOwnerGroup path (Groupname name) (Groupname n))
  for_ mperm $ \perm ->
    unless (Posix.fileMode status == perm) $
      throwIO (FitBadFileMode path perm (Posix.fileMode status))

-- | Errors encountered while running 'fit'
data FitError =
    FitBadFileContents FilePath FitContentsError
  | FitBadLinkSource FilePath String {- expected -} String {- actual -}
  | FitBadOwnerUser FilePath User {- expected -} User {- actual -}
  | FitBadOwnerGroup FilePath Group {- expected -} Group {- actual -}
  | FitBadFileMode FilePath Posix.FileMode {- expected -} Posix.FileMode {- actual -}
  | FitIOException FilePath IOErrorType
    deriving (Eq, Typeable, Generic)

-- | Expected/actual file contents mismatch
data FitContentsError =
    FitBadBinary ByteString ByteString
  | FitBadText Text Text
  | FitBadCopyOf FilePath
    deriving (Eq, Typeable, Generic)

instance Show FitError where
  show (FitBadFileContents path mismatch) = unlines $
    printf "Bad contents at ‘%s’" path : showCE mismatch
   where
    showCE :: FitContentsError -> [String]
    showCE (FitBadBinary expected actual) =
      [ "expected:"
      , printf "  %s" (show (ByteString.unpack expected))
      , "actual:"
      , printf "  %s" (show (ByteString.unpack actual))
      ]
    showCE (FitBadText expected actual) =
      [ "expected:"
      , printf "  %s" (show expected)
      , "actual:"
      , printf "  %s" (show actual)
      ]
    showCE (FitBadCopyOf f) =
      [ "expected:"
      , printf "  a copy of ‘%s’" f
      , "actual:"
      , "  something else"
      ]
  show (FitBadLinkSource path expected actual) = unlines $
    [ printf "Bad symlink source at ‘%s’" path
    , "expected:"
    , printf "  ‘%s’" expected
    , "actual:"
    , printf "  ‘%s’" actual
    ]
  show (FitBadOwnerUser path expected actual) = unlines $
    [ printf "Bad owner user id at ‘%s’" path
    , "expected:"
    , printf "  %s" (show expected)
    , "actual:"
    , printf "  %s" (show actual)
    ]
  show (FitBadOwnerGroup path expected actual) = unlines $
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

-- | Make the real directory layout from the description removing any previous contents
remake :: FilePath -> Layout a -> IO (Validation (NonEmpty MakeError) ())
remake p l = getCompose $
  Compose (validateIO (removeDirectoryRecursive p *> createDirectory p)) *> Compose (make p l)

makeIO :: FilePath -> F a -> IO ()
makeIO root = go where
  go (F (combine root -> fullpath) cs a _) = do
    case cs of
      Just (Binary bs) -> ByteString.writeFile fullpath bs
      Just (Text t) -> Text.writeFile fullpath t
      Just (CopyOf p) -> ByteStringLazy.readFile p >>= ByteStringLazy.writeFile fullpath
      Nothing -> ByteString.writeFile fullpath (ByteString.pack [])
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
  for_ muid $ \case
    UserID i ->
      Posix.setSymbolicLinkOwnerAndGroup path i (-1)
    Username name -> do
      i <- getUserID name
      Posix.setSymbolicLinkOwnerAndGroup path i (-1)
  for_ mgid $ \case
    GroupID i ->
      Posix.setSymbolicLinkOwnerAndGroup path (-1) i
    Groupname name -> do
      i <- getGroupID name
      Posix.setSymbolicLinkOwnerAndGroup path (-1) i
  for_ mperm $
    Posix.setFileMode path

-- | Errors encountered while running 'make'
data MakeError =
  MakeIOException FilePath IOErrorType
  deriving (Show, Eq, Typeable, Generic)

instance Exception MakeError where
  toException = SomeException
  fromException e'@(SomeException e)
    | Just ioe <- fromException e' =
        Just (MakeIOException (fromMaybe (ioeGetLocation ioe) (ioeGetFileName ioe)) (ioeGetErrorType ioe))
    | otherwise = cast e

getUserID :: String -> IO Posix.UserID
getUserID = fmap Posix.userID . Posix.getUserEntryForName

getUsername :: Posix.UserID -> IO String
getUsername = fmap Posix.userName . Posix.getUserEntryForID

getGroupID :: String -> IO Posix.GroupID
getGroupID = fmap Posix.groupID . Posix.getGroupEntryForName

getGroupname :: Posix.GroupID -> IO String
getGroupname = fmap Posix.groupName . Posix.getGroupEntryForID

-- | This type is isomorphic to 'Either' but its 'Applicative' instance accumulates errors
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

-- | Construct 'Validation' value from the list of errors
--
-- >>> fromErrors []
-- Result ()
--
-- >>> fromErrors Nothing
-- Result ()
--
-- >>> fromErrors "hello"
-- Error ('h' :| "ello")
--
-- >>> fromErrors (Just "hello")
-- Error ("hello" :| [])
fromErrors :: Foldable t => t e -> Validation (NonEmpty e) ()
fromErrors = go . toList
 where
  go [] = Result ()
  go (x : xs) = Error (x :| xs)
