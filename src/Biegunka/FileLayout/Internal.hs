{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.FileLayout.Internal
  ( FL(..)
  , file, file_, directory, directory_
  ) where

import Data.Text (Text)


-- | Abstract data type representing directory tree
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


-- | Declare file with specified contents
file ∷ FilePath → Text → FL ()
file x c = F x (Just c) (return ())


-- | Declare empty file
file_ ∷ FilePath → FL ()
file_ x = F x Nothing (return ())


-- | Declare directory with specified listing
directory ∷ FilePath → FL () → FL ()
directory x d = D x d (return ())


-- | Declare empty directory
directory_ ∷ FilePath → FL ()
directory_ x = D x (return ()) (return ())
