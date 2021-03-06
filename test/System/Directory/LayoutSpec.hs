{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Directory.LayoutSpec
  ( spec
  ) where

import Control.Lens
import Test.Hspec

import System.Directory.Layout


spec :: Spec
spec = do
  describe "directory-layout project structure" $ do
    rel examples project

project :: Layout ()
project = do
  dir "src" $ do
    dirs ["System", "Directory"] $ do
      dir "Layout" $ do
        file "Internal.hs"
        file "Interpreter.hs"
        file "QQ.hs"
      file "Layout.hs"
  dir "test" $ do
    dirs ["System", "Directory"] $ do
      dir "Layout" $ do
        file "InternalSpec.hs"
        file "InterpreterSpec.hs"
        file "QQSpec.hs"
      file "LayoutSpec.hs"
    file "Spec.hs"
      & contents ?~ [dedent|
        {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
        |]
    file "SpecHelper.hs"
    file "Doctest.hs"
  file "LICENSE"
  file "Guardfile"
  file "Gemfile"
  file "Setup.hs"
  file "directory-layout.cabal"

rel :: (FilePath -> Layout a -> b) -> Layout a -> b
rel f = f "."
