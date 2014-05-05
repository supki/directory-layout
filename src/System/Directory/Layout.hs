{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- | Directory layout DSL
module System.Directory.Layout
  ( -- * Describe layouts
    Layout
    -- ** Nodes
  , file
  , symlink
  , dir
  , dirs
  , emptydir
    -- ** Nodes augmentation
  , contents
  , Contents(..)
  , binary
  , text
  , dedent
  , copyOf
  , source
  , exists
  , User(..)
  , user
  , uid
  , username
  , Group(..)
  , group
  , gid
  , groupname
  , mode
  , anything
  , into
  , focus
    -- * Run layouts
  , module System.Directory.Layout.Interpreter
  ) where

import System.Directory.Layout.Internal
import System.Directory.Layout.Interpreter
import System.Directory.Layout.QQ
