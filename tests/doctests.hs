module Main where

import System.Wordexp.Simple
import Test.DocTest


main :: IO ()
main = do
  x <- wordexp "cabal-dev/packages-*.conf"
  case x of
    y:_ -> doctest ["-isrc", "-package-db=" ++ y, "src/System/Directory/Layout/Lens.hs"]
    _   -> doctest ["-isrc", "src/System/Directory/Layout/Lens.hs"]
