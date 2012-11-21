{-# LANGUAGE UnicodeSyntax #-}
module System.Directory.Layout
  ( -- * Layout declaration
    DL, file, file_, directory, directory_
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


-- | Declare file with specified contents
file ∷ FilePath → Text → DL ()
file x c = F x (Just c) (return ())


-- | Declare empty file
file_ ∷ FilePath → DL ()
file_ x = F x Nothing (return ())


-- | Declare directory with specified listing
directory ∷ FilePath → DL () → DL ()
directory x d = D x d (return ())


-- | Declare empty directory
directory_ ∷ FilePath → DL ()
directory_ x = D x (return ()) (return ())
