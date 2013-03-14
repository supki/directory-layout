{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.Exit (exitSuccess, exitFailure)

import System.Directory.Layout
import Test.HUnit hiding (assert)
import System.Process (rawSystem)


main ∷ IO ()
main = do
  z ← runTestTT tests
  if errors z + failures z > 0
    then exitFailure
    else exitSuccess
 where
  tests = TestList
    [ TestLabel "trivia1" testTrivia1
    , TestLabel "trivia2" testTrivia2
    , TestLabel "dual1" testDual1
    ]


testTrivia1 ∷ Test
testTrivia1 = TestCase $ do
  install
  check' >>= assertEqual "correct" []

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y"]
  check' >>= assertEqual "deleted y" [DirectoryDoesNotExist "x/y"]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/z"]
  check' >>= assertEqual "deleted z" [DirectoryDoesNotExist "./z"]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y/s"]
  check' >>= assertEqual "deleted s" [FileDoesNotExist "x/y/s"]

  install
  writeFile "directory-layout-test/z/w" "foo"
  check' >>= assertEqual "changed z/w" [FileWrongContents "z/w" "foo"]

  rawSystem "rm" ["-rf", "file-layout-test"]
  return ()
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


testTrivia2 ∷ Test
testTrivia2 = TestCase $ do
  rawSystem "mkdir" ["--parents", "directory-layout-test"]
  install
  check' >>= assertEqual "correct" []

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y"]
  check' >>= assertEqual "deleted y" [DirectoryDoesNotExist "x/y"]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/z"]
  check' >>= assertEqual "deleted z" [DirectoryDoesNotExist "./z"]

  install
  rawSystem "rm" ["-rf", "directory-layout-test/x/y/s"]
  check' >>= assertEqual "deleted s" [FileDoesNotExist "x/y/s"]

  install
  writeFile "directory-layout-test/z/w" "foo"
  check' >>= assertEqual "changed z/w" [FileWrongContents "z/w" "foo"]

  rawSystem "rm" ["-rf", "directory-layout-test"]
  return ()
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


testDual1 ∷ Test
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
  rawSystem "rm" ["-rf", "directory-layout-test"]
  return ()
 where
  test' s = make' s >> check' s
  make' s = make s "directory-layout-test"
  check' s = check s "directory-layout-test" >>= assertEqual "dual" []
