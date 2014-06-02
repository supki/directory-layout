{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Directory.Layout.QQSpec
  ( spec
  ) where

import Data.Text (Text)
import Test.Hspec

import System.Directory.Layout.QQ


spec :: Spec
spec = do
  describe "dedent" $ do
    it "gives reasonable result for the empty input" $
      [dedent||] `shouldBe` ""

    it "handles a single line input" $
      [dedent|hello|] `shouldBe` "hello"

    it "ignores the first line when it consists solely of spaces" $
      [dedent|
      hello|] `shouldBe` "hello"

    it "considers the first line when it has non-space characters" $
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

    it "ignores indentation in the last line if it's all spaces" $
      [dedent|
        hello
          world
        !
      |] `shouldBe` "hello\n  world\n!\n"

    it "saves empty lines at the beginning of the string" $
      [dedent|
        
        
        hello
          world
        !
      |] `shouldBe` "\n\nhello\n  world\n!\n"


    it "saves empty lines at the end of the string" $
      [dedent|
        hello
          world
        !
        
        
      |] `shouldBe` "hello\n  world\n!\n\n\n"

  describe "dedentSubst" $ do
    it "is like dedent but also supports variable substitution" $ do
      let hello = "bye" :: String
      [dedentSubst|
        #{hello}
          world
        !
        
        
      |] `shouldBe` "bye\n  world\n!\n\n\n"

    it "can be parsed into any IsString instance" $ do
      let hello = "bye" :: String
      [dedentSubst|
        #{hello}
          world
        !
      |] `shouldBe` ("bye\n  world\n!\n" :: Text)
