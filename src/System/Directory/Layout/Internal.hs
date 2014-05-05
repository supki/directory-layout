{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | directory-layout internals
module System.Directory.Layout.Internal where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Free
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Data (Data, Typeable)
import           Data.Foldable (Foldable)
import qualified Data.HashMap.Strict as HashMap
import           Data.Semigroup (Semigroup(..))
import           Data.String (IsString(..))
import           Data.Word (Word8)
import           Data.Text (Text)
#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (IsList(..))
#endif
import           GHC.Generics (Generic)
import           System.FilePath ((</>))
import qualified System.Posix as Posix

-- | Directory layout description
newtype Layout a = L { unL :: Free F a }
  deriving (Functor, Applicative, Monad, Foldable, Traversable, Typeable, Generic)

-- | The underlying 'Functor'
data F a =
    F String (Maybe Contents) Aux a
  | SL String FilePath Bool Aux a
  | D String a Aux a
  | E
    deriving (Eq, Functor, Foldable, Traversable, Typeable, Generic)

-- | Regular file contents
data Contents =
    Binary ByteString
  | Text Text
  | CopyOf FilePath
    deriving (Eq, Typeable, Data, Generic)

instance IsString Contents where
  fromString = Text . fromString

#if __GLASGOW_HASKELL__ >= 708
instance IsList Contents where
  type Item Contents = Word8
  fromList = Binary . ByteString.pack
  toList = error "Contents.toList: not implemented"
#endif

-- | Auxiliary data
data Aux = Aux (Maybe User) (Maybe Group) (Maybe Posix.FileMode)
    deriving (Show, Eq, Typeable, Generic)

-- | File owner
data User =
    UserID Posix.UserID
  | Username String
    deriving (Show, Eq, Typeable, Generic)

instance IsString User where
  fromString = Username

-- | File group
data Group =
    GroupID Posix.GroupID
  | Groupname String
    deriving (Show, Eq, Typeable, Generic)

instance IsString Group where
  fromString = Groupname

-- | Equality check does not care about the order the files are listed insofar
-- they are consistent, i.e. different things aren't named the same
instance Eq (Layout a) where
  L xs == L ys = go "." xs == go "." ys
   where
    go root (Free t@(F n _ _ m)) =
      HashMap.singleton (root </> n) (() <$ t) <> go root m
    go root (Free t@(SL n _ _ _ m)) =
      HashMap.singleton (root </> n) (() <$ t) <> go root m
    go root (Free t@(D n is _ m)) =
      HashMap.singleton (root </> n) (() <$ t) <> go (root </> n) is <> go root m
    go _ (Free E) = HashMap.empty
    go _ (Pure _) = HashMap.empty

instance Semigroup (Layout a) where
  (<>) = (>>)

-- | Regular file with some contents or empty
--
-- >>> let layout = file "foo"
file :: String -> Layout ()
file name = L (liftF (F name anything defaux ()))

-- | Symbolic link
--
-- >>> let layout = symlink "foo" "bar"
symlink
  :: String   -- link's name
  -> FilePath -- link's source
  -> Layout ()
symlink name s = L (liftF (SL name s False defaux ()))

-- | Directory
--
-- >>> :{
-- let layout = dir "foo" $ do
--       file "bar"
--       file "baz"
-- :}
dir :: String -> Layout a -> Layout ()
dir name is = L (Free (D name (unL is >> liftF E) defaux (Pure ())))

-- | Empty directory
--
-- >>> let layout = emptydir "foo"
emptydir :: String -> Layout ()
emptydir name = dir name (return ())

-- | A nested list of directories
--
-- >>> :{
-- let layout = dirs ["foo", "bar"] $ do
--                file "qux"
--                file "quux"
-- :}
dirs :: [String] -> Layout () -> Layout ()
dirs names l = foldr dir l names

-- | The default (empty) auxiliary data
defaux :: Aux
defaux = Aux Nothing Nothing Nothing

-- | An optic into file contents
contents :: Traversal' (Layout a) (Maybe Contents)
contents f (L (Free (F n cs a x@(Pure _)))) = f cs <&> \cs' -> L (Free (F n cs' a x))
contents _ l = pure l
{-# INLINE contents #-}

-- | Binary contents
--
-- >>> let layout = file "foo" & contents ?~ binary (ByteString.pack [1..10])
binary :: ByteString -> Contents
binary = Binary

-- | Plain text contents
--
-- >>> let layout = file "foo" & contents ?~ text (Data.Text.pack "hello")
text :: Text -> Contents
text = Text

-- | Contents are the copy of whose of the real file
--
-- >>> let layout = file "foo" & contents ?~ copyOf "/home/user/.vimrc"
copyOf :: FilePath -> Contents
copyOf = CopyOf

-- | Anything
--
-- >>> let layout = file "foo" & contents .~ anything
-- >>> let layout = file "foo" & user .~ anything
anything :: Maybe a
anything = Nothing

-- | An optic into symbolic link source
--
-- >>> symlink "foo" "bar" ^? source
-- Just "bar"
source :: Traversal' (Layout a) String
source f (L (Free (SL n s e a x@(Pure _)))) = f s <&> \s' -> L (Free (SL n s' e a x))
source _ l = pure l
{-# INLINE source #-}

-- | An optic into symbolic link source expected existence
--
-- >>> let layout = symlink "foo" "bar" & exists .~ True
exists :: Traversal' (Layout a) Bool
exists f (L (Free (SL n s e a x@(Pure _)))) = f e <&> \e' -> L (Free (SL n s e' a x))
exists _ l = pure l
{-# INLINE exists #-}

-- | An optic into file auxiliary data
aux :: Traversal' (Layout a) Aux
aux f (L (Free (F n cs a x@(Pure _)))) = f a <&> \a' -> L (Free (F n cs a' x))
aux f (L (Free (SL n s e a x@(Pure _)))) = f a <&> \a' -> L (Free (SL n s e a' x))
aux f (L (Free (D n is a x@(Pure _)))) = f a <&> \a' -> L (Free (D n is a' x))
aux _ l = pure l
{-# INLINE aux #-}

-- | An optic into file owner
--
-- >>> let layout = file "foo" & user ?~ uid 0
user :: Traversal' (Layout a) (Maybe User)
user = aux . \f (Aux x y z) -> f x <&> \x' -> Aux x' y z

-- | Set the file owner by uid
uid :: Posix.UserID -> User
uid = UserID

-- | Set the file owner by username
--
-- >>> let layout = file "foo" & user ?~ username "root"
username :: String -> User
username = Username

-- | An optic into file group
--
-- >>> let layout = file "foo" & group ?~ gid 0
group :: Traversal' (Layout a) (Maybe Group)
group = aux . \f (Aux x y z) -> f y <&> \y' -> Aux x y' z

-- | Set the file group by groupname
gid :: Posix.GroupID -> Group
gid = GroupID

-- | Set the file group by groupname
--
-- >>> let layout = file "foo" & group ?~ groupname "wheel"
groupname :: String -> Group
groupname = Groupname

-- | An optic into file mode
--
-- >>> let layout = file "foo" & mode ?~ 0o100777
mode :: Traversal' (Layout a) (Maybe Posix.FileMode)
mode = aux . \f (Aux x y z) -> f z <&> \z' -> Aux x y z'

-- | An optic into directory contents
innards :: Traversal' (Layout a) (Layout a)
innards f (L (Free (D n is a x@(Pure _)))) = fmap unL (f (L is)) <&> \is' -> L (Free (D n is' a x))
innards _ l = pure l
{-# INLINE innards #-}

-- | An optic into the directory contents of the particular directory
--
-- >>> :{
-- dirs ["foo", "bar", "baz"] (symlink "qux" "quux")
--   ^? into "foo".into "bar".into "baz".focus "qux".source
-- :}
-- Just "quux"
into :: String -> Traversal' (Layout ()) (Layout ())
into s = focus s.innards

-- | An optic into the particular node
focus :: String -> Traversal' (Layout ()) (Layout ())
focus k f = fmap L . go . unL where
  go (Free (F n cs a x))
    | n == k =
      g (liftF (F n cs a ())) <&> \(Free (F _ cs' a' _)) -> Free (F n cs' a' x)
    | otherwise =
      go x <&> Free . F n cs a
  go (Free (D n is a x))
    | n == k =
      g (Free (D n is a (Pure ()))) <&> \(Free (D _ is' a' _)) -> Free (D n is' a' x)
    | otherwise =
      go x <&> Free . D n is a
  go (Free (SL n s e a x))
    | n == k =
      g (liftF (SL n s e a ())) <&> \(Free (SL _ s' e' a' _)) -> Free (SL n s' e' a' x)
    | otherwise =
      go x <&> Free . SL n s e a
  go (Free E) = pure (Free E)
  go (Pure x) = pure (Pure x)

  g = fmap unL . f . L
{-# INLINE focus #-}
