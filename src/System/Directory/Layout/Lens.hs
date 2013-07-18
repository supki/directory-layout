{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | "Control.Lens" based extractors for 'Layout'
module System.Directory.Layout.Lens
  ( -- * Usage
    -- $setup
    text, name, names, next, file, directory, node
  ) where

import Control.Applicative ((<$>), (<*>), pure)

import Control.Lens
import Data.Text (Text)

import System.Directory.Layout.Internal (Node(..), Layout)


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> let layout = F "foo" (T "not empty" ()) (D "bar" (F "baz" (E ()) (F "quux" (T "something" ()) (E ()))) (F "swaks" (E ()) (E ())))


-- | Target 'Text' from the current 'Layout' top (if possible)
--
-- >>> layout ^? text
-- Nothing
-- >>> layout ^? file "foo" . text
-- Just "not empty"
-- >>> layout ^? directory "bar" . file "quux" . text
-- Just "something"
text :: Prism' Layout Text
text = prism' (\t -> T t ()) $ \s -> case s of
  T t _ -> Just t
  _     -> Nothing
{-# INLINE text #-}

-- | Target 'FilePath' from the current 'Layout' top (if possible)
--
-- >>> layout ^? name
-- Just "foo"
-- >>> layout ^? directory "bar" . name
-- Just "baz"
-- >>> layout ^? directory "quux" . name
-- Nothing
-- >>> layout & name .~ "boo"
-- F "boo" (T "not empty" ()) (D "bar" (F "baz" (E ()) (F "quux" (T "something" ()) (E ()))) (F "swaks" (E ()) (E ())))
name :: Traversal' Layout FilePath
name f = go
 where
  go (E x)   = pure (E x)
  go (T t x) = pure (T t x)
  go (F n l x) = f n <&> \n' -> F n' l x
  go (D n l x) = f n <&> \n' -> D n' l x
{-# INLINE name #-}

-- | Target all 'Filpath's from current 'Layout' layer
--
-- >>> layout ^? names
-- Just "foo"
-- >>> layout ^.. names
-- ["foo","bar","swaks"]
-- >>> layout ^.. directory "bar" . names
-- ["baz","quux"]
-- >>> layout & directory "bar" . names %~ reverse
-- F "foo" (T "not empty" ()) (D "bar" (F "zab" (E ()) (F "xuuq" (T "something" ()) (E ()))) (F "swaks" (E ()) (E ())))
names :: Traversal' Layout FilePath
names f = go
 where
  go (E x)   = pure (E x)
  go (T t x) = pure (T t x)
  go (F n l x) = (\n' x' -> F n' l x') <$> f n <*> go x
  go (D n l x) = (\n' x' -> D n' l x') <$> f n <*> go x
{-# INLINE names #-}

-- | Target next 'Node'
--
-- >>> layout ^? name
-- Just "foo"
-- >>> layout ^? next . name
-- Just "bar"
-- >>> layout ^? next . next . name
-- Just "swaks"
-- >>> layout ^? next . next . next . name
-- Nothing
next :: Traversal' Layout Layout
next f = go
 where
  go (E x)   = pure (E x)
  go (T t x) = pure (T t x)
  go (F n l x) = f x <&> \x' -> F n l x'
  go (D n l x) = f x <&> \x' -> D n l x'
{-# INLINE next #-}

-- | Target 'Layout' under the current 'Layout' top if it happens to be a file
--
-- >>> layout ^? file "biz"
-- Nothing
-- >>> layout ^? file "swaks"
-- Just (E ())
-- >>> layout ^? directory "bar" . file "baz"
-- Just (E ())
file :: FilePath -> IndexedTraversal' FilePath Layout Layout
file k f = go
 where
  go (E x)      = pure (E x)
  go (T t x)    = pure (T t x)
  go (F k' l x)
    | k == k'   = indexed f k l <&> \l' -> F k' l' x
    | otherwise = go x <&> \x' -> F k' l x'
  go (D n l x)  = go x <&> \x' -> D n l x'
{-# INLINE file #-}

-- | Target 'Layout' under the current 'Layout' top if it happens to be a directory
--
-- >>> layout ^? directory "foo"
-- Nothing
-- >>> layout ^? directory "bar"
-- Just (F "baz" (E ()) (F "quux" (T "something" ()) (E ())))
directory :: FilePath -> IndexedTraversal' FilePath Layout Layout
directory k f = go
 where
  go (E x)      = pure (E x)
  go (T t x)    = pure (T t x)
  go (F n l x)  = go x <&> \x' -> F n l x'
  go (D k' l x)
    | k == k'   = indexed f k l <&> \l' -> D k' l' x
    | otherwise = go x <&> \x' -> D k' l x'
{-# INLINE directory #-}

-- | Target 'Layout' under the current 'Layout' top
--
-- >>> layout ^? node "foo"
-- Just (T "not empty" ())
-- >>> layout ^? node "bar"
-- Just (F "baz" (E ()) (F "quux" (T "something" ()) (E ())))
-- >>> layout ^? node "what"
-- Nothing
node :: FilePath -> IndexedTraversal' FilePath Layout Layout
node k f = go
 where
  go (E x)      = pure (E x)
  go (T t x)    = pure (T t x)
  go (F k' l x)
    | k == k'   = indexed f k l <&> \l' -> F k' l' x
    | otherwise = go x <&> \x' -> F k' l x'
  go (D k' l x)
    | k == k'   = indexed f k l <&> \l' -> D k' l' x
    | otherwise = go x <&> \x' -> D k' l x'
{-# INLINE node #-}
