{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.FileLayout
  ( FL(..)
  , file, fileText, dir
  ) where

import           Data.Text (Text)


data FL f
  = E f
  | F FilePath (Maybe Text) (FL f)
  | D FilePath (FL ()) (FL f)
    deriving (Show)


instance Functor FL where
  fmap f (E x) = E (f x)
  fmap f (F fp c x) = F fp c (fmap f x)
  fmap f (D fp x y) = D fp x (fmap f y)


instance Monad FL where
  return = E
  E x >>= f = f x
  F fp c x >>= f = F fp c (x >>= f)
  D fp x y >>= f = D fp x (y >>= f)


file ∷ FilePath → FL ()
file x = F x Nothing (return ())


fileText ∷ FilePath → Text → FL ()
fileText x c = F x (Just c) (return ())


dir ∷ FilePath → FL () → FL ()
dir x d = D x d (return ())
