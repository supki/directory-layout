{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- | Directory layout DSL
module System.Directory.Layout
  ( -- * Layout description
    Layout
  , file
  , symlink
  , dir
  , dirs
  , emptydir
    -- ** Augment nodes
  , contents
  , Contents
  , binary
  , text
  , anything
  , source
  , exists
  , user
  , group
  , mode
  , into
  , focus
  , module System.Directory.Layout.Interpreter
  ) where

import System.Directory.Layout.Internal
import System.Directory.Layout.Interpreter
