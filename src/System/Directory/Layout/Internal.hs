module System.Directory.Layout.Internal
  ( DL(..), Layout
  ) where

import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))

import Data.Default (Default(..))
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)


-- | But type synonym is nicer
type Layout = DL ()


-- | Abstract data type representing directory tree is nice
data DL f
  = E f
  | T Text f
  | F FilePath Layout (DL f)
  | D FilePath Layout (DL f)
    deriving (Show, Read, Eq)

instance Default a => Default (DL a) where
  def = E def
  {-# INLINE def #-}

instance Semigroup a => Semigroup (DL a) where
  E a      <> E b = E (a <> b)
  E _      <> b   = b
  T _ _    <> b   = b
  F f t l  <> b   = F f t (l  <> b)
  D f l l' <> b   = D f l (l' <> b)
  {-# INLINE (<>) #-}

instance (Default a, Semigroup a) => Monoid (DL a) where
  mempty = def
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Functor DL where
  fmap f (E x)      = E (f x)
  fmap f (T t x)    = T t (f x)
  fmap f (F fp c x) = F fp c (fmap f x)
  fmap f (D fp x y) = D fp x (fmap f y)
  {-# INLINE fmap #-}

instance Apply DL where
  E f      <.> E x      = E (f x)
  E f      <.> T t x    = T t (f x)
  T t f    <.> E x      = T t (f x)
  T t f    <.> T _ x    = T t (f x)
  f        <.> F fp c x = F fp c (f <.> x)
  f        <.> D fp l x = D fp l (f <.> x)
  F fp c f <.> x        = F fp c (f <.> x)
  D fp l f <.> x        = D fp l (f <.> x)
  {-# INLINE (<.>) #-}

instance Applicative DL where
  pure = E
  {-# INLINE pure #-}

  (<*>) = (<.>)
  {-# INLINE (<*>) #-}

instance Bind DL where
  E x      >>- f = f x
  T _ x    >>- f = f x
  F fp c x >>- f = F fp c (x >>- f)
  D fp x y >>- f = D fp x (y >>- f)
  {-# INLINE (>>-) #-}

instance Monad DL where
  return = pure
  {-# INLINE return #-}

  (>>=) = (>>-)
  {-# INLINE (>>=) #-}
