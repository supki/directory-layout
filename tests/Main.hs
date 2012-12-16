{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.String (IsString)
import System.Exit (exitSuccess, exitFailure)

import System.Directory.Layout
import System.Directory.Layout.Internal
import Test.HUnit hiding (assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Process (rawSystem)


deriving instance Eq a ⇒ Eq (DL a)


main ∷ IO ()
main = do
  z ← runTestTT tests
  quickCheck testArbitrary1
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


testArbitrary1 ∷ Gen Prop
testArbitrary1 = monadicIO . forAllM arbitrary $ \l → do
  run (rawSystem "mkdir" ["--parents", "directory-layout-test"])
  xs ← run (make l "directory-layout-test")
  ys ← run (check l "directory-layout-test")
  run (rawSystem "rm" ["-rf", "directory-layout-test"])
  assert $ null xs && null ys


testParsing1 ∷ Test
testParsing1 = TestCase $ do
  assertEqual "test0" (return layout0) (layout test0)
  assertEqual "test0'" (return layout0) (layout' test0)
  assertEqual "test1" (return layout1) (layout test1)
  assertEqual "test1'" (return layout1) (layout' test1)
  assertEqual "test2" (return layout2) (layout test2)
  assertEqual "test2'" (return layout2) (layout' test2)
  assertEqual "test3" (return layout3) (layout test3)
  assertEqual "test3'" (return layout3) (layout' test3)
  assertEqual "test4" (return layout4) (layout test4)
  assertEqual "test4'" (return layout4) (layout' test4)
  assertEqual "test5" (return layout5) (layout test5)
  assertEqual "test5'" (return layout5) (layout' test5)
  assertEqual "test6" (return layout6) (layout test6)
  assertEqual "test6'" (return layout6) (layout' test6)
  assertEqual "test7" (return layout7) (layout test7)
  assertEqual "test7'" (return layout7) (layout' test7)
  assertEqual "test8" (return layout8) (layout test8)
  assertEqual "test8'" (return layout8) (layout' test8)
  assertEqual "test9" (return layout9) (layout test9)
  assertEqual "test9'" (return layout9) (layout' test9)
  assertEqual "test10" (return layout10) (layout test10)
  assertEqual "test10'" (return layout10) (layout' test10)
 where
  test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10 ∷ IsString s ⇒ s
  layout0  = return ()
  test0 = ""
  layout1  = file_ "file"
  test1 = "file\n"
  layout2  = directory_ "dir"
  test2 = "dir/\n"
  layout3  = directory "dir" (file_ "file")
  test3 = "dir/\n file\n"
  layout4  = directory_ "dir" >> file_ "file"
  test4 = "dir/\nfile\n"
  layout5  = file_ "file" >> directory_ "dir"
  test5 = "file\ndir/\n"
  layout6  = directory "dir" (directory "dir" (file_ "file"))
  test6 = "dir/\n dir/\n  file\n"
  layout7  = directory "dir" (directory "dir" (file_ "file") >> file_ "file")
  test7 = "dir/\n dir/\n  file\n file\n"
  layout8  = directory "dir" (directory "dir" (file_ "file")) >> file_ "file"
  test8 = "dir/\n dir/\n  file\nfile\n"
  layout9  = directory "dir" (directory "dir" (directory_ "dir")) >> file_ "file"
  test9 = "dir/\n dir/\n  dir/\nfile\n"
  layout10 = do
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
  assertEqual "test0" (return layout0) (layout test0)
  assertEqual "test0'" (return layout0) (layout' test0)
  assertEqual "test1" (return layout1) (layout test1)
  assertEqual "test1'" (return layout1) (layout' test1)
  assertEqual "test2" (return layout2) (layout test2)
  assertEqual "test2'" (return layout2) (layout' test2)
  assertEqual "test3" (return layout3) (layout test3)
  assertEqual "test3'" (return layout3) (layout' test3)
  assertEqual "test4" (return layout4) (layout test4)
  assertEqual "test4'" (return layout4) (layout' test4)
  assertEqual "test5" (return layout5) (layout test5)
  assertEqual "test5'" (return layout5) (layout' test5)
 where
  test0, test1, test2, test3, test4, test5 ∷ IsString s ⇒ s
  layout0  = file "file" "n\nn\n"
  test0 = "file\n n\n n\n \n"
  layout1  = file "file" "n\nn\n" >> file "file" "t\nt\n"
  test1 = "file\n n\n n\n \nfile\n t\n t\n \n"
  layout2  = file "file" "n\nn\n" >> file_ "file"
  test2 = "file\n n\n n\n \nfile\n"
  layout3  = file_ "file" >> file "file" "n\nn\n"
  test3 = "file\nfile\n n\n n\n \n"
  layout4  = file_ "file" >> file "file" "n\nn\n" >> file_ "file"
  test4 = "file\nfile\n n\n n\n \nfile\n"
  layout5  = do
    directory "dir" $ do
      file_ "file"
      file "file" "n\nn\n"
      file "file" "t\nt\n"
    directory_ "dir"
  test5 = "dir/\n file\n file\n  n\n  n\n  \n file\n  t\n  t\n  \ndir/\n"
