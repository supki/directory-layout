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
  , Contents(..)
  , binary
  , text
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
  , module System.Directory.Layout.Interpreter
  , module System.Directory.Layout.QQ
  ) where

import System.Directory.Layout.Internal
import System.Directory.Layout.Interpreter
import System.Directory.Layout.QQ
