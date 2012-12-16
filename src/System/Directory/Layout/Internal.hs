{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Directory.Layout.Internal
  ( DL(..), Layout
  ) where

import Control.Applicative
import Control.Arrow

import Data.Text (Text)
import Test.QuickCheck


-- | Abstract data type representing directory tree is nice
data DL f
  = E f
  | F FilePath (Maybe Text) (DL f)
  | D FilePath (DL ()) (DL f)
    deriving (Show, Read)


-- | But type synonym is nicer
type Layout = DL ()


instance Functor DL where
  fmap f (E x) = E (f x)
  fmap f (F fp c x) = F fp c (fmap f x)
  fmap f (D fp x y) = D fp x (fmap f y)


instance Monad DL where
  return = E
  E x >>= f = f x
  F fp c x >>= f = F fp c (x >>= f)
  D fp x y >>= f = D fp x (y >>= f)


-- Make arbitrary layout of reasonable size
-- Frequencies are pretty /arbitrary/ chosen with layout construction termination in mind
instance Arbitrary a ⇒ Arbitrary (DL a) where
  arbitrary = snd <$> generator 0
   where
    generator ∷ Arbitrary a ⇒ Int → Gen (Int, DL a)
    generator n = frequency
      [ (8, do
          (n', g') ← generator (succ n)
          generator n' <&> second (D (show n) g'))
      , (20, generator (succ n) <&> second (F (show n) Nothing))
      , (10, arbitrary <&> \r → (n, E r))
      ]
     where
      (<&>) = flip fmap
