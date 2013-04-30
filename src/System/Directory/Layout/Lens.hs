{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module System.Directory.Layout.Lens
  ( text, file, directory
  ) where

import Control.Applicative (pure)

import Control.Lens
import Data.Text (Text)

import System.Directory.Layout.Internal (DL(..), Layout)


text :: Prism Layout Layout Text Text
text = prism' (\t -> T t ()) $ \s -> case s of
  T t _ -> Just t
  _     -> Nothing
{-# INLINE text #-}


file :: FilePath -> IndexedTraversal' FilePath (DL b) Layout
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


directory :: FilePath -> IndexedTraversal' FilePath (DL b) Layout
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
