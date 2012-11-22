{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Directory.Layout.Internal
  ( DL(..)
  ) where

import Data.Text (Text)


-- | Abstract data type representing directory tree
data DL f
  = E f
  | F FilePath (Maybe Text) (DL f)
  | D FilePath (DL ()) (DL f)
    deriving (Show, Read)


instance Functor DL where
  fmap f (E x) = E (f x)
  fmap f (F fp c x) = F fp c (fmap f x)
  fmap f (D fp x y) = D fp x (fmap f y)


instance Monad DL where
  return = E
  E x >>= f = f x
  F fp c x >>= f = F fp c (x >>= f)
  D fp x y >>= f = D fp x (y >>= f)