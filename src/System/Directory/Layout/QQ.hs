{-# LANGUAGE TemplateHaskell #-}
-- | Convenience quasiquoter to ease the pain working with multiline strings
module System.Directory.Layout.QQ (dedent) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (liftString)
import Language.Haskell.TH (Q, Exp)


-- | A handy quasiquoter to work with the multiline file contents
--
-- Strips the longest common leading spaces segment. All spacey characters are treated
-- equally. The first line is ignored if it's spaces only.
--
-- >>> :set -XQuasiQuotes
-- >>> :{
-- putStr [dedent|
--   hello
--     world
--     !
--   |]
-- :}
-- hello
--   world
--   !
dedent :: QuasiQuoter
dedent = quoter $
  liftString . intercalate "\n" . stripCommonLeadingWhitespace . dropFirst (all isSpace) . lines

dropFirst :: (a -> Bool) -> [a] -> [a]
dropFirst _ [] = []
dropFirst p (x : xs)
  | p x = xs
  | otherwise = x : xs

stripCommonLeadingWhitespace :: [String] -> [String]
stripCommonLeadingWhitespace xs = map (drop (commonLeadingWhitespace xs)) xs

commonLeadingWhitespace :: [String] -> Int
commonLeadingWhitespace = minimumOr 0 . map (length . takeWhile isSpace)

minimumOr :: Ord a => a -> [a] -> a
minimumOr n [] = n
minimumOr _ xs = minimum xs

quoter :: (String -> Q Exp) -> QuasiQuoter
quoter quote = QuasiQuoter
  { quoteExp  = quote
  , quotePat  = failure "patterns"
  , quoteType = failure "types"
  , quoteDec  = failure "declarations"
  }
 where
  failure kind =
    fail $ "this quasiquoter does not support splicing " ++ kind
