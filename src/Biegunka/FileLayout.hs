{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.FileLayout
  ( -- * Layout declaration
    FL, file, file_, directory, directory_
    -- * Layout construction
  , FLRunWarning(..), run
    -- * Layout check
  , FLCheckFailure(..), check
    -- * Flt parser
  , flt, flt'
  ) where

import Data.Text (Text)

import Biegunka.FileLayout.Internal (FL(..))
import Biegunka.FileLayout.Check (FLCheckFailure(..), check)
import Biegunka.FileLayout.Run (FLRunWarning(..), run)
import Biegunka.FileLayout.Flt (flt, flt')


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
