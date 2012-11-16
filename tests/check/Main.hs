{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.Exit (exitSuccess, exitFailure)

import Biegunka.FileLayout
import Test.HUnit
import System.Process (rawSystem)


main ∷ IO ()
main = do
  z ← runTestTT tests
  if errors z + failures z > 0
    then exitFailure
    else exitSuccess
 where
  tests = TestList [TestLabel "trivia1" testTrivia1, TestLabel "trivia2" testTrivia2]


testTrivia1 ∷ Test
testTrivia1 = TestCase $ do
  install
  check' >>= assertEqual "correct" []

  install
  rawSystem "rm" ["-rf", "file-layout-test/x"]
  check' >>= assertEqual "deleted x" [DirectoryDoesNotExist "./x"]

  install
  rawSystem "rm" ["-rf", "file-layout-test/x/y"]
  check' >>= assertEqual "deleted y" [DirectoryDoesNotExist "x/y"]

  install
  rawSystem "rm" ["-rf", "file-layout-test/x/z"]
  check' >>= assertEqual "deleted z" [FileDoesNotExist "x/z"]

  rawSystem "rm" ["-rf", "file-layout-test"]
  return ()
 where
  install = do
    rawSystem "mkdir" ["--parents", "file-layout-test"]
    rawSystem "mkdir" ["--parents", "file-layout-test/x/y"]
    rawSystem "touch" ["file-layout-test/x/z"]

  script =
    directory "x" $ do
      directory_ "y"
      file_ "z"

  check' = check script "file-layout-test"


testTrivia2 ∷ Test
testTrivia2 = TestCase $ do
  install
  check' >>= assertEqual "correct" []

  install
  rawSystem "rm" ["-rf", "file-layout-test/x/y"]
  check' >>= assertEqual "deleted y" [DirectoryDoesNotExist "x/y"]

  install
  rawSystem "rm" ["-rf", "file-layout-test/z"]
  check' >>= assertEqual "deleted z" [DirectoryDoesNotExist "./z"]

  install
  rawSystem "rm" ["-rf", "file-layout-test/x/y/s"]
  check' >>= assertEqual "deleted s" [FileDoesNotExist "x/y/s"]

  install
  writeFile "file-layout-test/z/w" "foo"
  check' >>= assertEqual "changed z/w" [FileWrongContents "z/w" "foo"]

  rawSystem "rm" ["-rf", "file-layout-test"]
  return ()
 where
  install = do
    rawSystem "mkdir" ["--parents", "file-layout-test"]
    rawSystem "mkdir" ["--parents", "file-layout-test/x/y"]
    rawSystem "mkdir" ["--parents", "file-layout-test/z"]
    rawSystem "touch" ["file-layout-test/x/y/s"]
    rawSystem "touch" ["file-layout-test/x/y/v"]
    rawSystem "touch" ["file-layout-test/z/w"]
    writeFile "file-layout-test/z/w" "text"

  script = do
    directory "x" $
      directory "y" $ do
        file_ "s"
        file_ "v"
    directory "z" $
      file "w" "text"

  check' = check script "file-layout-test"
