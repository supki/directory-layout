{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Directory.Layout.InternalSpec
  ( spec
  ) where

import Control.Lens
import Control.Monad.Free
import Data.Char (toUpper)
import Test.Hspec

import System.Directory.Layout.Internal

deriving instance Show Contents
deriving instance Show a => Show (F a)
deriving instance Show a => Show (Layout a)


spec :: Spec
spec = do
  describe "Eq instance" $ do
    it "does not care about the order inside a layer" $
      l1 `shouldBe` l2

    it "does care about the file contents" $
      (l1 & into "bar".focus "baz".contents .~ "baz'es innards") == l2 `shouldBe` False

    it "does care about the link source" $
      (l1 & focus "xyzzy".source.mapped %~ toUpper) == l2 `shouldBe` False

  describe "contents" $ do
    it "can change contents of the file to the specified text" $
      (file "foo" & contents .~ "bar") `shouldBe` L (liftF (F "foo" "bar" defaux ()))

    it "can change contents of the file to the specified bytes" $ do
      (file "foo" & contents .~ [1..10]) `shouldBe` L (liftF (F "foo" [1..10] defaux ()))

    it "can change contents of the file to anything!" $
      (file "foo" & contents .~ anything) `shouldBe` L (liftF (F "foo" W defaux ()))

  describe "source" $ do
    it "can change source of the symlink to the specified string" $
      (symlink "foo" "bar" & source .~ "baz") `shouldBe` symlink "foo" "baz"

  describe "ix" $ do
    it "can focus a file" $ do
      let l = do
            file "foo"
            file "bar"
            file "baz"

      (l & focus "bar".contents .~ "qux") `shouldBe` do
         file "foo"
         file "bar"
           & contents .~ "qux"
         file "baz"

    it "can focus a file inside the directory" $ do
      let l = do
            dir "foo" $
              dir "bar" $
                file "baz"

      (l & into "foo".into "bar".focus "baz".contents .~ "qux") `shouldBe` do
         dir "foo" $
           dir "bar" $
             file "baz"
               & contents .~ "qux"

    it "can focus a directory" $ do
      let l = do
            dir "foo" $
              dir "bar" $
                file "baz"

      (l & into "foo".focus "bar".mode ?~ 0o040777) `shouldBe` do
         dir "foo" $ do
           dir "bar" $ do
             file "baz"
          & mode ?~ 0o040777

l1, l2 :: Layout ()
l1 = do
  file "foo"
  dir "bar" $ do
    file "baz"
    file "qux"
      & contents .~ "qux's innards"
    file "quux"
      & contents .~ "quux's innards"
  file "xyz"
  symlink "xyzzy" "yzzyx"
l2 = do
  dir "bar" $ do
    file "baz"
    file "quux"
      & contents .~ "quux's innards"
    file "qux"
      & contents .~ "qux's innards"
  file "foo"
  symlink "xyzzy" "yzzyx"
  file "xyz"
