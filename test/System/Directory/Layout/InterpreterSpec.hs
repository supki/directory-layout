{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Directory.Layout.InterpreterSpec
  ( spec
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString as ByteString
import           Data.Foldable (traverse_)
import           Data.List.NonEmpty (NonEmpty)
import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import           System.FilePath ((</>))
import           System.IO.Error (doesNotExistErrorType, permissionErrorType)
import qualified System.Posix as Posix
import           Test.Hspec

import           SpecHelper
import           System.Directory.Layout hiding (spec)


spec :: Spec
spec = do
  describe "Validation" $
    it "combines failures with the Semigroup instance's (<>)" $
      traverse_ tonel ([1, 2, 3, 4] :: [Int]) `shouldBe` fromErrors [1,2,3,4]

  describe "fit" $ do
    it "tests regular file existence" $ do
      temporary $ \p -> do
        r <- fit p $ do
          file "foo"
        r `shouldBe` fromErrors [FitIOException (p </> "foo") doesNotExistErrorType]

    it "does not test regular file contents" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") "foo"
        r <- fit p $ do
          file "foo"
        r `shouldBe` fromErrors []

    it "tests text file existence" $ do
      temporary $ \p -> do
        r <- fit p $ do
          file "foo"
            & contents ?~ text "bar"
        r `shouldBe` fromErrors [FitIOException (p </> "foo") doesNotExistErrorType]

    it "tests text file contents" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") "foo"
        r <- fit p $ do
          file "foo"
            & contents ?~ text "bar"
        r `shouldBe` fromErrors
          [ FitBadFileContents (p </> "foo") $
              FitBadText "bar" "foo"
          ]

    it "tests text file contents specified with the quasiquoter" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") "foo"
        r <- fit p $ do
          file "foo"
            & contents ?~ [dedent|
                foo
                bar
                |]
        r `shouldBe` fromErrors
          [ FitBadFileContents (p </> "foo") $
              FitBadText "foo\nbar\n" "foo"
          ]

    it "tests binary file existence" $ do
      temporary $ \p -> do
        r <- fit p $ do
          file "foo"
            & contents ?~ binary (ByteString.pack [1, 2, 3, 4])
        r `shouldBe` fromErrors [FitIOException (p </> "foo") doesNotExistErrorType]

    it "tests binary file contents" $ do
      temporary $ \p -> do
        ByteString.writeFile (p </> "foo") (ByteString.pack [5, 6, 7, 8])
        r <- fit p $ do
          file "foo"
            & contents ?~ binary (ByteString.pack [1, 2, 3, 4])
        r `shouldBe` fromErrors
         [ FitBadFileContents (p </> "foo") $
             FitBadBinary (ByteString.pack [1, 2, 3, 4]) (ByteString.pack [5, 6, 7, 8])
         ]

    it "tests copy file contents" $ do
      temporary $ \p -> do
        ByteString.writeFile (p </> "foo") (ByteString.pack [1, 2, 3, 4])
        ByteString.writeFile (p </> "bar") (ByteString.pack [5, 6, 7, 8])
        r <- fit p $ do
          file "foo"
            & contents ?~ copyOf (p </> "bar")
        r `shouldBe` fromErrors
          [ FitBadFileContents (p </> "foo") $
              FitBadCopyOf (p </> "bar")
          ]

    it "tests copy file contents" $ do
      temporary $ \p -> do
        ByteString.writeFile (p </> "foo") (ByteString.pack [1, 2, 3, 4])
        ByteString.writeFile (p </> "bar") (ByteString.pack [1, 2, 3, 4])
        r <- fit p $ do
          file "foo"
            & contents ?~ copyOf (p </> "bar")
        r `shouldBe` fromErrors []

    it "tests symbolic link existence" $ do
      temporary $ \p -> do
        r <- fit p $ do
          symlink "foo" "bar"
        r `shouldBe` fromErrors [FitIOException (p </> "foo") doesNotExistErrorType]

    it "tests symbolic link source" $ do
      temporary $ \p -> do
        Posix.createSymbolicLink "baz" (p </> "foo")
        r <- fit p $ do
          symlink "foo" "bar"
        r `shouldBe` fromErrors [FitBadLinkSource (p </> "foo") "bar" "baz"]

    it "combines multiple errors on one layer" $ do
      temporary $ \p -> do
        writeFile (p </> "bar") "qux"
        r <- fit p $ do
          file "foo"
          file "bar"
            & contents ?~ text "quux"
          file "baz"
        r `shouldBe` fromErrors
          [ FitIOException (p </> "foo") doesNotExistErrorType
          , FitBadFileContents (p </> "bar") $
              FitBadText "quux" "qux"
          , FitIOException (p </> "baz") doesNotExistErrorType
          ]

    it "combines multiple errors on multiple layers" $ do
      temporary $ \p -> do
        createDirectoryIfMissing True (p </> "xyz" </> "xyzzy")
        writeFile (p </> "xyz" </> "xyzzy" </> "bar") "qux"
        r <- fit p $ do
          dirs ["xyz", "xyzzy"] $ do
            file "foo"
            file "bar"
              & contents ?~ text "quux"
          dir "boo" $
            file "hoo"
        r `shouldBe` fromErrors
          [ FitIOException (p </> "xyz" </> "xyzzy" </> "foo") doesNotExistErrorType
          , FitBadFileContents (p </> "xyz" </> "xyzzy" </> "bar") $
              FitBadText "quux" "qux"
          , FitIOException (p </> "boo") doesNotExistErrorType
          , FitIOException (p </> "boo" </> "hoo") doesNotExistErrorType
          ]

    it "tests file owner user id" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") ""
        r <- fit p $ do
          file "foo"
            & user ?~ uid 0
        r `shouldBe` fromErrors [FitBadOwnerUser (p </> "foo") (uid 0) (uid 1000)]

    it "tests file owner user name" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") ""
        n <- Posix.getEffectiveUserName
        r <- fit p $ do
          file "foo"
            & user ?~ username "root"
        r `shouldBe` fromErrors [FitBadOwnerUser (p </> "foo") (username "root") (username n)]

    it "tests file owner group id" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") ""
        r <- fit p $ do
          file "foo"
            & group ?~ gid 0
        r `shouldBe` fromErrors [FitBadOwnerGroup (p </> "foo") (gid 0) (gid 1000)]

    it "tests file owner group id" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") ""
        n <- Posix.getEffectiveUserName
        r <- fit p $ do
          file "foo"
            & group ?~ groupname "root"
        r `shouldBe` fromErrors [FitBadOwnerGroup (p </> "foo") (groupname "root") (groupname n)]

    it "tests file permissions" $ do
      temporary $ \p -> do
        writeFile (p </> "foo") ""
        Posix.setFileMode (p </> "foo") 0o100644
        r <- fit p $ do
          file "foo"
            & mode ?~ 0o100777
        r `shouldBe` fromErrors [FitBadFileMode (p </> "foo") 0o100777 0o100644]

    it "tests symbolic link's source exists" $ do
      temporary $ \p -> do
        let l = symlink "boo" "hoo"
        Posix.createSymbolicLink "hoo" (p </> "boo")
        fit p l `shouldReturn`
          fromErrors []
        fit p (l & exists .~ True) `shouldReturn`
          fromErrors [FitIOException (p </> "boo") doesNotExistErrorType]
        writeFile (p </> "hoo") ""
        fit p (l & exists .~ True) `shouldReturn`
          fromErrors []

    it "does not throw exceptions if root directory does not exist" $
      temporary $ \p -> do
        removeDirectoryRecursive p
        r <- fit p $
          file "foo"
            & contents ?~ text "bar"
        r `shouldBe` fromErrors
          [ FitIOException (p </> "foo") doesNotExistErrorType
          ]

  describe "make" $ do
    -- examples use 'fit' because if the above spec passes then
    -- we can be reasonably sure 'fit' works as expected
    it "creates a file" $
      makefit $
        file "foo"

    it "creates a file with the specified text" $ do
      makefit $
        file "foo"
          & contents ?~ text "bar"

    it "creates a copy of the file with the specified text" $ do
      temporary $ \p -> do
        writeFile (p </> "qux") "quux"
        makefit $
          file "foo"
            & contents ?~ copyOf (p </> "qux")

    it "creates two files with the specified text" $ do
      makefit $ do
        file "foo"
          & contents ?~ text "bar"
        file "qux"
          & contents ?~ text "quux"

    it "creates two files and a symlink" $ do
      makefit $ do
        file "foo"
          & contents ?~ text "bar"
        file "qux"
          & contents ?~ text "quux"
        symlink "boo" "hoo"

    it "creates a directory with a file" $ do
      makefit $
        dir "foo" $
          file "bar"

    it "creates a directory with two files" $ do
      makefit $
        dir "foo" $ do
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
          file "quux"
            & contents ?~ binary (ByteString.pack [98, 121, 101])

    it "creates a nested directory with two files" $ do
      makefit $
        dirs ["foo", "bar"] $ do
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
          file "quux"
            & contents ?~ binary (ByteString.pack [98, 121, 101])

    it "creates a nested directory with two files and a directory" $ do
      makefit $
        dirs ["foo", "bar"] $ do
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
          file "quux"
            & contents ?~ binary (ByteString.pack [98, 121, 101])

    it "creates a tree of directories with files" $ do
      makefit $
        dir "foo" $ do
          dir "bar" $ do
            file "qux"
              & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
            file "quux"
              & contents ?~ binary (ByteString.pack [98, 121, 101])
          dir "baz" $
            symlink "boo" "hoo"

    it "changes the user id of the file owner" $ do
      temporary $ \p -> do
        r <- make p $
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
            & user ?~ uid 0
        r `shouldBe` fromErrors [MakeIOException (p </> "qux") permissionErrorType]

    it "changes the user id of the symbolic link owner" $ do
      temporary $ \p -> do
        r <- make p $
          symlink "foo" "bar"
            & user ?~ uid 0
        r `shouldBe` fromErrors [MakeIOException (p </> "foo") permissionErrorType]

    it "changes the user name of the file owner" $ do
      temporary $ \p -> do
        r <- make p $
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
            & user ?~ username "root"
        r `shouldBe` fromErrors [MakeIOException (p </> "qux") permissionErrorType]

    it "changes the user name of the symbolic link owner" $ do
      temporary $ \p -> do
        r <- make p $
          symlink "foo" "bar"
            & user ?~ username "root"
        r `shouldBe` fromErrors [MakeIOException (p </> "foo") permissionErrorType]

    it "changes the user id of the directory owner" $ do
      temporary $ \p -> do
        r <- make p $
          emptydir "boo"
            & user ?~ uid 0
        r `shouldBe` fromErrors [MakeIOException (p </> "boo") permissionErrorType]

    it "changes the user name of the directory owner" $ do
      temporary $ \p -> do
        r <- make p $
          emptydir "boo"
            & user ?~ username "root"
        r `shouldBe` fromErrors [MakeIOException (p </> "boo") permissionErrorType]

    it "changes the group id of the file owner" $ do
      temporary $ \p -> do
        r <- make p $
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
            & group ?~ gid 0
        r `shouldBe` fromErrors [MakeIOException (p </> "qux") permissionErrorType]

    it "changes the group name of the file owner" $ do
      temporary $ \p -> do
        r <- make p $
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
            & group ?~ groupname "root"
        r `shouldBe` fromErrors [MakeIOException (p </> "qux") permissionErrorType]

    it "changes the group id of the symbolic link owner" $ do
      temporary $ \p -> do
        r <- make p $
          symlink "foo" "bar"
            & group ?~ gid 0
        r `shouldBe` fromErrors [MakeIOException (p </> "foo") permissionErrorType]

    it "changes the group name of the symbolic link owner" $ do
      temporary $ \p -> do
        r <- make p $
          symlink "foo" "bar"
            & group ?~ groupname "root"
        r `shouldBe` fromErrors [MakeIOException (p </> "foo") permissionErrorType]

    it "changes the group id of the directory owner" $ do
      temporary $ \p -> do
        r <- make p $
          file "boo"
            & group ?~ gid 0
        r `shouldBe` fromErrors [MakeIOException (p </> "boo") permissionErrorType]

    it "changes the group name of the directory owner" $ do
      temporary $ \p -> do
        r <- make p $
          file "boo"
            & group ?~ groupname "root"
        r `shouldBe` fromErrors [MakeIOException (p </> "boo") permissionErrorType]

    it "changes the file permissions" $ do
      makefit $
        file "qux"
          & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
          & mode ?~ 0o100777

    it "changes the directory permissions" $ do
      makefit $
        emptydir "boo"
          & mode ?~ 0o040777

    it "tolerates redundant directories" $ do
      makefit $ do
        dir "foo" $
          file "qux"
            & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
        dir "foo" $
          file "quux"
            & contents ?~ binary (ByteString.pack [98, 121, 101])

    it "the latter write wins" $ do
      temporary $ \p -> do
        let l = do
              dir "foo" $
                file "qux"
                  & contents ?~ binary (ByteString.pack [104, 101, 108, 108, 111])
              dir "foo" $
                file "qux"
                  & contents ?~ binary (ByteString.pack [98, 121, 101])
        _ <- make p l
        fit p l `shouldReturn` fromErrors
          [ FitBadFileContents (p </> "foo" </> "qux") $ FitBadBinary
              (ByteString.pack [104, 101, 108, 108, 111])
              (ByteString.pack [98, 121, 101])
          ]

    it "does not throw exceptions if root directory does not exist" $
      temporary $ \p -> do
        removeDirectoryRecursive p
        r <- make p $
          file "foo"
            & contents ?~ text "bar"
        r `shouldBe` fromErrors [MakeIOException (p </> "foo") doesNotExistErrorType]

  describe "remake" $ do
    it "does not throw exceptions if root directory does not exist, but it checks its existence" $
      temporary $ \p -> do
        removeDirectoryRecursive p
        r <- remake p $
          file "foo"
            & contents ?~ text "bar"
        r `shouldBe` fromErrors
          [ MakeIOException p doesNotExistErrorType
          , MakeIOException (p </> "foo") doesNotExistErrorType
          ]

    it "does not remove symlink sources" $
      temporary $ \p -> do
        temporary $ \p' -> do
          make p' (file "quux" & contents ?~ "symlink source") `shouldReturn` fromErrors []
          make p (symlink "qux" (p' </> "quux")) `shouldReturn` fromErrors []
          remake p (file "foo" & contents ?~ text "bar") `shouldReturn` fromErrors []
          fit p' (file "quux" & contents ?~ "symlink source") `shouldReturn` fromErrors []

tonel :: a -> Validation (NonEmpty a) b
tonel = Error . pure

makefit :: Layout a -> IO ()
makefit l = temporary $ \p -> do
  make p l `shouldReturn` fromErrors []
  fit p l `shouldReturn` fromErrors []
