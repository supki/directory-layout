{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | "Control.Lens" based extractors for 'Layout'
module System.Directory.Layout.Lens
  ( -- * Usage
    -- $setup
    name, names, next, file, directory
  ) where

import Control.Applicative ((<$>), (<*>), pure)

import Control.Lens
import Data.Text (Text)

import System.Directory.Layout.Internal (Node(..), Layout)


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> let layout = F "foo" (Just "not empty") (D "bar" (F "baz" Nothing (F "quux" (Just "something") (E ()))) (F "swaks" Nothing (E ())))


-- | Target 'FilePath' from the current 'Layout' top (if possible)
--
-- >>> layout ^? name
-- Just "foo"
-- >>> layout ^? directory "bar" . name
-- Just "baz"
-- >>> layout ^? directory "quux" . name
-- Nothing
-- >>> layout & name .~ "boo"
-- F "boo" (Just "not empty") (D "bar" (F "baz" Nothing (F "quux" (Just "something") (E ()))) (F "swaks" Nothing (E ())))
name :: Traversal' Layout FilePath
name f = go
 where
  go (E x)   = pure (E x)
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
-- F "foo" (Just "not empty") (D "bar" (F "zab" Nothing (F "xuuq" (Just "something") (E ()))) (F "swaks" Nothing (E ())))
names :: Traversal' Layout FilePath
names f = go
 where
  go (E x)   = pure (E x)
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
  go (F n l x) = f x <&> \x' -> F n l x'
  go (D n l x) = f x <&> \x' -> D n l x'
{-# INLINE next #-}

-- | Target 'Layout' under the current 'Layout' top if it happens to be a file
--
-- >>> layout ^? file "biz"
-- Nothing
-- >>> layout ^? file "swaks"
-- Just Nothing
-- >>> layout ^? directory "bar" . file "baz"
-- Just Nothing
file :: FilePath -> IndexedTraversal' FilePath Layout (Maybe Text)
file k f = go
 where
  go (E x)      = pure (E x)
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
-- Just (F "baz" Nothing (F "quux" (Just "something") (E ())))
directory :: FilePath -> IndexedTraversal' FilePath Layout Layout
directory k f = go
 where
  go (E x)      = pure (E x)
  go (F n l x)  = go x <&> \x' -> F n l x'
  go (D k' l x)
    | k == k'   = indexed f k l <&> \l' -> D k' l' x
    | otherwise = go x <&> \x' -> D k' l x'
{-# INLINE directory #-}
