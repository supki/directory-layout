{-# LANGUAGE OverloadedStrings #-}
module System.Directory.LayoutSpec (spec) where

import Control.Exception (finally)
import System.Directory.Layout
import System.IO.Error
import System.Process (rawSystem)
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec = do
 fromHUnitTest (TestLabel "trivia1" testTrivia1)
 fromHUnitTest (TestLabel "trivia2" testTrivia2)
 fromHUnitTest (TestLabel "dual1" testDual1)

testTrivia1 :: Test
testTrivia1 = TestCase $ do
  install
  check' >>= assertEqual "correct" []

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y"]
  check' >>= assertEqual "deleted y"
    [ DE doesNotExistErrorType "x/y"
    , FE doesNotExistErrorType "x/y/s"
    , FE doesNotExistErrorType "x/y/v"
    ]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/z"]
  check' >>= assertEqual "deleted z"
    [ DE doesNotExistErrorType "z"
    , RF doesNotExistErrorType "z/w" "text"
    ]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y/s"]
  check' >>= assertEqual "deleted s" [FE doesNotExistErrorType "x/y/s"]

  install
  writeFile "directory-layout-test/z/w" "foo"
  check' >>= assertEqual "changed z/w" [RF userErrorType "z/w" "text"]
 `finally`
  rawSystem "rm" ["-rf", "directory-layout-test"]
 where
  install = do
    rawSystem "mkdir" ["--parents", "directory-layout-test"]
    rawSystem "mkdir" ["--parents", "directory-layout-test/x/y"]
    rawSystem "mkdir" ["--parents", "directory-layout-test/z"]
    rawSystem "touch" ["directory-layout-test/x/y/s"]
    rawSystem "touch" ["directory-layout-test/x/y/v"]
    rawSystem "touch" ["directory-layout-test/z/w"]
    writeFile "directory-layout-test/z/w" "text"

  script = do
    directory "x" $
      directory "y" $ do
        file_ "s"
        file_ "v"
    directory "z" $
      file "w" "text"

  check' = check script "directory-layout-test"


testTrivia2 :: Test
testTrivia2 = TestCase $ do
  rawSystem "mkdir" ["--parents", "directory-layout-test"]
  install
  check' >>= assertEqual "correct" []

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y"]
  check' >>= assertEqual "deleted y"
    [ DE doesNotExistErrorType "x/y"
    , FE doesNotExistErrorType "x/y/s"
    , FE doesNotExistErrorType "x/y/v"
    ]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/z"]
  check' >>= assertEqual "deleted z"
    [ DE doesNotExistErrorType "z"
    , RF doesNotExistErrorType "z/w" "text"
    ]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y/s"]
  check' >>= assertEqual "deleted s" [FE doesNotExistErrorType "x/y/s"]

  install
  writeFile "directory-layout-test/z/w" "foo"
  check' >>= assertEqual "changed z/w" [RF userErrorType "z/w" "text"]
 `finally`
  rawSystem "rm" ["-rf", "directory-layout-test"]
 where
  install = make script "directory-layout-test"

  script = do
    directory "x" $
      directory "y" $ do
        file_ "s"
        file_ "v"
    directory "z" $
      file "w" "text"

  check' = check script "directory-layout-test"


testDual1 :: Test
testDual1 = TestCase $ do
  rawSystem "mkdir" ["--parents", "directory-layout-test"]
  let s = do
        directory "x" $ do
          directory_ "xx"
          directory "xy" $
            file "xyx" "test1"
          file "xz" "test2"
          file "xz'" "test3"
          file_ "xz''"
        directory "y" $ do
          file "yx" "test4"
          file "yy" "test5"
          file_ "yz"
        directory_ "z"
  test' s
 `finally`
  rawSystem "rm" ["-rf", "directory-layout-test"]
 where
  test' s = make' s >> check' s
  make' s = make s "directory-layout-test"
  check' s = check s "directory-layout-test" >>= assertEqual "dual" []
