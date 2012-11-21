{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.String (IsString)
import System.Exit (exitSuccess, exitFailure)

import Biegunka.FileLayout
import Biegunka.FileLayout.Internal
import Test.HUnit
import System.Process (rawSystem)


deriving instance Eq a ⇒ Eq (FL a)


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
    , TestLabel "parsing1" testParsing1
    , TestLabel "parsing2" testParsing2
    ]


testTrivia1 ∷ Test
testTrivia1 = TestCase $ do
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


testTrivia2 ∷ Test
testTrivia2 = TestCase $ do
  rawSystem "mkdir" ["--parents", "file-layout-test"]
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
  install = run script "file-layout-test"

  script = do
    directory "x" $
      directory "y" $ do
        file_ "s"
        file_ "v"
    directory "z" $
      file "w" "text"

  check' = check script "file-layout-test"

testDual1 ∷ Test
testDual1 = TestCase $ do
  rawSystem "mkdir" ["--parents", "file-layout-test"]
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
  rawSystem "rm" ["-rf", "file-layout-test"]
  return ()
 where
  test' s = run' s >> check' s
  run' s = run s "file-layout-test"
  check' s = check s "file-layout-test" >>= assertEqual "dual" []


testParsing1 ∷ Test
testParsing1 = TestCase $ do
  assertEqual "test0" (return flt0) (flt test0)
  assertEqual "test0'" (return flt0) (flt' test0)
  assertEqual "test1" (return flt1) (flt test1)
  assertEqual "test1'" (return flt1) (flt' test1)
  assertEqual "test2" (return flt2) (flt test2)
  assertEqual "test2'" (return flt2) (flt' test2)
  assertEqual "test3" (return flt3) (flt test3)
  assertEqual "test3'" (return flt3) (flt' test3)
  assertEqual "test4" (return flt4) (flt test4)
  assertEqual "test4'" (return flt4) (flt' test4)
  assertEqual "test5" (return flt5) (flt test5)
  assertEqual "test5'" (return flt5) (flt' test5)
  assertEqual "test6" (return flt6) (flt test6)
  assertEqual "test6'" (return flt6) (flt' test6)
  assertEqual "test7" (return flt7) (flt test7)
  assertEqual "test7'" (return flt7) (flt' test7)
  assertEqual "test8" (return flt8) (flt test8)
  assertEqual "test8'" (return flt8) (flt' test8)
  assertEqual "test9" (return flt9) (flt test9)
  assertEqual "test9'" (return flt9) (flt' test9)
  assertEqual "test10" (return flt10) (flt test10)
  assertEqual "test10'" (return flt10) (flt' test10)
 where
  test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10 ∷ IsString s ⇒ s
  flt0  = return ()
  test0 = ""
  flt1  = file_ "file"
  test1 = "file\n"
  flt2  = directory_ "dir"
  test2 = "dir/\n"
  flt3  = directory "dir" (file_ "file")
  test3 = "dir/\n file\n"
  flt4  = directory_ "dir" >> file_ "file"
  test4 = "dir/\nfile\n"
  flt5  = file_ "file" >> directory_ "dir"
  test5 = "file\ndir/\n"
  flt6  = directory "dir" (directory "dir" (file_ "file"))
  test6 = "dir/\n dir/\n  file\n"
  flt7  = directory "dir" (directory "dir" (file_ "file") >> file_ "file")
  test7 = "dir/\n dir/\n  file\n file\n"
  flt8  = directory "dir" (directory "dir" (file_ "file")) >> file_ "file"
  test8 = "dir/\n dir/\n  file\nfile\n"
  flt9  = directory "dir" (directory "dir" (directory_ "dir")) >> file_ "file"
  test9 = "dir/\n dir/\n  dir/\nfile\n"
  flt10 = do
    file_ "file"
    directory "dir" $
      directory "dir" $
        file_ "file"
    directory_ "dir"
    directory "dir" $
      file_ "file"
    file_ "file"
  test10 = "file\ndir/\n dir/\n  file\ndir/\ndir/\n file\nfile\n"


testParsing2 ∷ Test
testParsing2 = TestCase $ do
  assertEqual "test0" (return flt0) (flt test0)
  assertEqual "test0'" (return flt0) (flt' test0)
  assertEqual "test1" (return flt1) (flt test1)
  assertEqual "test1'" (return flt1) (flt' test1)
  assertEqual "test2" (return flt2) (flt test2)
  assertEqual "test2'" (return flt2) (flt' test2)
  assertEqual "test3" (return flt3) (flt test3)
  assertEqual "test3'" (return flt3) (flt' test3)
  assertEqual "test4" (return flt4) (flt test4)
  assertEqual "test4'" (return flt4) (flt' test4)
  assertEqual "test5" (return flt5) (flt test5)
  assertEqual "test5'" (return flt5) (flt' test5)
 where
  test0, test1, test2, test3, test4, test5 ∷ IsString s ⇒ s
  flt0  = file "file" "n\nn\n"
  test0 = "file\n n\n n\n \n"
  flt1  = file "file" "n\nn\n" >> file "file" "t\nt\n"
  test1 = "file\n n\n n\n \nfile\n t\n t\n \n"
  flt2  = file "file" "n\nn\n" >> file_ "file"
  test2 = "file\n n\n n\n \nfile\n"
  flt3  = file_ "file" >> file "file" "n\nn\n"
  test3 = "file\nfile\n n\n n\n \n"
  flt4  = file_ "file" >> file "file" "n\nn\n" >> file_ "file"
  test4 = "file\nfile\n n\n n\n \nfile\n"
  flt5  = do
    directory "dir" $ do
      file_ "file"
      file "file" "n\nn\n"
      file "file" "t\nt\n"
    directory_ "dir"
  test5 = "dir/\n file\n file\n  n\n  n\n  \n file\n  t\n  t\n  \ndir/\n"
