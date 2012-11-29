{-# LANGUAGE UnicodeSyntax #-}
module System.Directory.Layout
  ( -- * Layout declaration
    Layout, DL, file, file_, directory, directory_
    -- * Layout construction
  , DLMakeWarning(..), make
    -- * Layout verification
  , DLCheckFailure(..), check
    -- * Layout parsers
  , layout, layout'
  ) where

import Data.Text (Text)

import System.Directory.Layout.Internal (DL(..))
import System.Directory.Layout.Check (DLCheckFailure(..), check)
import System.Directory.Layout.Make (DLMakeWarning(..), make)
import System.Directory.Layout.Parser (layout, layout')


-- | Type synonym is nicer
type Layout = DL ()


-- | Declare file with specified contents
file ∷ FilePath → Text → Layout
file x c = F x (Just c) (return ())


-- | Declare empty file
file_ ∷ FilePath → Layout
file_ x = F x Nothing (return ())


-- | Declare directory with specified listing
directory ∷ FilePath → Layout → Layout
directory x d = D x d (return ())


-- | Declare empty directory
directory_ ∷ FilePath → Layout
directory_ x = D x (return ()) (return ())
