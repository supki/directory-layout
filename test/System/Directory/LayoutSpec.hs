{-# LANGUAGE OverloadedStrings #-}
module System.Directory.LayoutSpec
  ( spec
  ) where

import           Control.Lens
import           Test.Hspec

import           System.Directory.Layout hiding (spec)
import qualified System.Directory.Layout.Interpreter as L


spec :: Spec
spec = do
  describe "directory-layout project structure" $ do
    rel L.spec project

project :: Layout ()
project = do
  dir "src" $ do
    dirs ["System", "Directory"] $ do
      dir "Layout" $ do
        file "Internal.hs"
        file "Interpreter.hs"
      file "Layout.hs"
  dir "test" $ do
    dirs ["System", "Directory"] $ do
      dir "Layout" $ do
        file "InternalSpec.hs"
        file "InterpreterSpec.hs"
      file "LayoutSpec.hs"
    file "Spec.hs"
      & contents .~ "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n"
    file "SpecHelper.hs"
  file "LICENSE"
  file "Guardfile"
  file "Setup.hs"
  file "directory-layout.cabal"

rel :: (FilePath -> Layout a -> b) -> Layout a -> b
rel f = f "."
