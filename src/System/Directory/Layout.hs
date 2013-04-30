{-# LANGUAGE UnicodeSyntax #-}
module System.Directory.Layout
  ( -- * Layout declaration
    DL, Layout, file, file_, directory, directory_
    -- * Layout construction
  , DLMakeWarning(..), make
    -- * Layout verification
  , DLCheckFailure(..), check
  ) where

import Data.Default (def)
import Data.Text (Text)

import System.Directory.Layout.Internal (DL(..), Layout)
import System.Directory.Layout.Check (DLCheckFailure(..), check)
import System.Directory.Layout.Make (DLMakeWarning(..), make)


-- | Declare file with specified contents
file ∷ FilePath → Text → Layout
file x t = F x (T t ()) def
{-# INLINE file #-}


-- | Declare empty file
file_ ∷ FilePath → Layout
file_ x = F x def def
{-# INLINE file_ #-}


-- | Declare directory with specified listing
directory ∷ FilePath → Layout → Layout
directory x d = D x d def
{-# INLINE directory #-}


-- | Declare empty directory
directory_ ∷ FilePath → Layout
directory_ x = D x def def
{-# INLINE directory_ #-}
