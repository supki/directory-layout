{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.FileLayout.Internal
  ( FL(..)
  ) where

import Data.Text (Text)


-- | Abstract data type representing directory tree
data FL f
  = E f
  | F FilePath (Maybe Text) (FL f)
  | D FilePath (FL ()) (FL f)
    deriving (Show, Read)


instance Functor FL where
  fmap f (E x) = E (f x)
  fmap f (F fp c x) = F fp c (fmap f x)
  fmap f (D fp x y) = D fp x (fmap f y)


instance Monad FL where
  return = E
  E x >>= f = f x
  F fp c x >>= f = F fp c (x >>= f)
  D fp x y >>= f = D fp x (y >>= f)
