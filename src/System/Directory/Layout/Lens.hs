{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module System.Directory.Layout.Lens
  ( text, file, directory
  ) where

import Control.Applicative (pure)

import Control.Lens
import Data.Text (Text)

import System.Directory.Layout.Internal (DL(..), Layout)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import           Control.Lens
-- >>> let layout = F "foo" (T "not empty" ()) (D "bar" (F "baz" (E ()) (F "quux" (T "something" ()) (E ()))) (F "swaks" (E ()) (E ())))


-- | Get 'Text' out of the current 'Layout' (if possible)
--
-- >>> layout ^? text
-- Nothing
-- >>> layout ^? file "foo" . text
-- Just "not empty"
-- >>> layout ^? directory "bar" . file "quux" . text
-- Just "something"
text :: Prism Layout Layout Text Text
text = prism' (\t -> T t ()) $ \s -> case s of
  T t _ -> Just t
  _     -> Nothing
{-# INLINE text #-}


-- | Look into the file in the current 'Layout' (if possible)
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
    | otherwise = go x
  go (D _ _ x)  = go x
  {-# INLINE go #-}
{-# INLINE file #-}


-- | Go into the directory in the current 'Layout' (if possible)
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
  go (F _ _ x)  = go x
  go (D k' l x)
    | k == k'   = indexed f k l <&> \l' -> D k' l' x
    | otherwise = go x
  {-# INLINE go #-}
{-# INLINE directory #-}
