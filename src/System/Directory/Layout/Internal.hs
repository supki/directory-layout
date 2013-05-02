-- | Free monad based directory layouts
module System.Directory.Layout.Internal
  ( DL(..), Layout
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)
import Data.Monoid (Monoid(..))

import Data.Default (Default(..))
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)


-- | Type synonym to save some acrobatics
type Layout = DL ()


-- | Representation of directory layouts
--
-- Invariants:
--
--  * 'F' second argument is never @D _ _ _@ or @F _ _ _@ itself
--
--  * 'F' third argument is never @T _ _@
--
--  * 'D' second argument is never @T _ _@
--
--  * 'D' third argument is never @T _ _@
data DL a
  = E !a                        -- ^ Emptyness, nothing found here
  | T !Text !a                  -- ^ File contents
  | F !FilePath !Layout !(DL a) -- ^ File node
  | D !FilePath !Layout !(DL a) -- ^ Directory node
    deriving (Show, Read, Eq, Ord)

instance Default a => Default (DL a) where
  def = E def
  {-# INLINE def #-}

instance Semigroup (DL a) where
  E _      <> b   = b
  T _ _    <> b   = b
  F f t l  <> b   = F f t (l  <> b)
  D f l l' <> b   = D f l (l' <> b)
  {-# INLINE (<>) #-}

instance Default a => Monoid (DL a) where
  mempty = def
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Functor DL where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Apply DL where
  E f      <.> E x      = E (f x)
  E f      <.> T t x    = T t (f x)
  T t f    <.> E x      = T t (f x)
  T t f    <.> T _ x    = T t (f x)
  F fp c f <.> x        = F fp c (f <.> x)
  D fp l f <.> x        = D fp l (f <.> x)
  f        <.> F fp c x = F fp c (f <.> x)
  f        <.> D fp l x = D fp l (f <.> x)
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

instance Foldable DL where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable DL where
  traverse f (E x)      = E      <$> f x
  traverse f (T t x)    = T t    <$> f x
  traverse f (F fp t x) = F fp t <$> traverse f x
  traverse f (D fp x y) = D fp x <$> traverse f y
  {-# INLINE traverse #-}
