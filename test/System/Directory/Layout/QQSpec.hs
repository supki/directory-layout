{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module System.Directory.Layout.QQSpec
  ( spec
  ) where

import Data.Text (Text)
import Test.Hspec

import System.Directory.Layout.QQ


spec :: Spec
spec =
  describe "dedent" $ do
    it "handles the empty input" $
      [dedent||] `shouldBe` ""

    it "handles a simple input" $
      [dedent|hello|] `shouldBe` "hello"

    it "ignores the first line when it consists solely of spaces" $
      [dedent|
      hello|] `shouldBe` "hello"

    it "does not ignore the first line when it has any non-space characters" $
      [dedent|hello
      world|] `shouldBe` "hello\n      world"

    it "removes the minimum common amount of indentation" $
      [dedent|
      hello
        world
      !
      |] `shouldBe` "hello\n  world\n!\n"

    it "can be parsed into any IsString instance" $
      [dedent|
      hello
        world
      !
      |] `shouldBe` ("hello\n  world\n!\n" :: Text)
