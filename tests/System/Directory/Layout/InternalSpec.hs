module System.Directory.Layout.InternalSpec where

import Control.Monad ((>=>))
import Data.Semigroup ((<>))
import System.Directory.Layout
import Test.Hspec


spec :: Spec
spec = do
  describe "basic equalities" $ do
    it "is sane" $ do
      layout_0 == layout_1  `shouldBe` False
      layout_1 == layout_1' `shouldBe` True

  describe "laws" $ do
    describe "left and right indentities" $ do
      it "holds for layout 0" $ do
        (layout_0 >>= return) `shouldBe` layout_0
        (return () >>= \() -> layout_0) `shouldBe` layout_0

      it "holds for layout 1" $ do
        (layout_1 >>= return) `shouldBe` layout_1
        (return () >>= \() -> layout_1) `shouldBe` layout_1

    describe "associativity" $ do
      it "holds for layouts 2 and 3" $ do
        layout_2 (>>) `shouldBe` layout_3 (>>)

      it "holds for layouts 4 and 5" $ do
        layout_4 () `shouldBe` layout_5 ()

    describe "semigroup instance associativity" $ do
      it "holds for layouts 2 and 3" $ do
        layout_2 (<>) `shouldBe` layout_3 (<>)

layout_0, layout_1, layout_1' :: Layout
layout_0 = do
  file_ "foo"
  file_ "bar"
  file_ "baz"
layout_1 = do
  file_ "foo"
  file_ "bar"
  directory "quux" $ do
    file_ "zem"
    file_ "zek"
  file_ "baz"
layout_1' = do
  file_ "foo"
  file_ "bar"
  directory "quux" $ do
    file_ "zek"
    file_ "zem"
  file_ "baz"

layout_2, layout_3 :: (Layout -> Layout -> Layout) -> Layout
layout_2 (#) =
  (file_ "foo" # file_ "bar") # file_ "baz"
layout_3 (#) =
  file_ "foo" # (file_ "bar" # file_ "baz")

layout_4, layout_5 :: () -> Layout
layout_4 =
  (const (file_ "foo") >=> const (file_ "bar")) >=> const (file_ "baz")
layout_5 =
  const (file_ "foo") >=> (const (file_ "bar") >=> const (file_ "baz"))
