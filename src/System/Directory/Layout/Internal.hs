{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Directory.Layout.Internal
  ( DL(..), Layout
  ) where

import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))

import Data.Default (Default(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)


-- | But type synonym is nicer
type Layout = DL ()


-- | Abstract data type representing directory tree is nice
data DL f
  = E f
  | F FilePath (Maybe Text) (DL f)
  | D FilePath (DL ()) (DL f)
    deriving (Show, Read, Eq)

instance Default a => Default (DL a) where
  def = E def

instance Semigroup a => Semigroup (DL a) where
  E a      <> E b = E (a <> b)
  E _      <> b   = b
  F f t l  <> b   = F f t (l  <> b)
  D f l l' <> b   = D f l (l' <> b)

instance (Default a, Semigroup a) => Monoid (DL a) where
  mempty = def
  mappend = (<>)

instance Functor DL where
  fmap f (E x)      = E (f x)
  fmap f (F fp c x) = F fp c (fmap f x)
  fmap f (D fp x y) = D fp x (fmap f y)

instance Applicative DL where
  pure = E
  E f      <*> E x      = E (f x)
  f        <*> F fp c x = F fp c (f <*> x)
  f        <*> D fp l x = D fp l (f <*> x)
  F fp c f <*> x        = F fp c (f <*> x)
  D fp l f <*> x        = D fp l (f <*> x)

instance Monad DL where
  return = E
  E x      >>= f = f x
  F fp c x >>= f = F fp c (x >>= f)
  D fp x y >>= f = D fp x (y >>= f)
