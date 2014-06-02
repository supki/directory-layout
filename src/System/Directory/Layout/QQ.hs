{-# LANGUAGE TemplateHaskell #-}
-- | Convenience quasiquoter to ease the pain working with multiline strings
module System.Directory.Layout.QQ
  ( dedent
  , dedentSubst
  ) where

import           Control.Applicative
import           Data.Char (isSpace)
import           Data.Foldable (Foldable(..), toList)
import           Data.List (intercalate)
import           Data.Sequence (Seq, ViewL(..), ViewR(..), (|>))
import qualified Data.Sequence as Seq
import           Data.String (fromString)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax (liftString)
import           Language.Haskell.TH (Q, Exp)
import           Prelude hiding (foldr)
import           System.Command.QQ (substituteVars, quoter)


-- $setup
-- >>> :set -XQuasiQuotes

-- | A handy quasiquoter to work with the multiline file contents
--
-- Strips the longest common leading spaces segment. All spacey characters are treated
-- equally. The first line is ignored if it's spaces only.
--
-- >>> :{
-- putStr [dedent|
--   hello
--     world
--     !
-- |]
-- :}
-- hello
--   world
--   !
dedent :: QuasiQuoter
dedent = dedentWith liftString

-- | 'dedent' with variable substitution
--
-- >>> let hello = "bye" :: String
-- >>> :{
-- putStr [dedentSubst|
--   #{hello}
--     world
--     !
-- |]
-- :}
-- bye
--   world
--   !
dedentSubst :: QuasiQuoter
dedentSubst = dedentWith $ \x -> [e| fromString $(substituteVars x) |]

dedentWith :: (String -> Q Exp) -> QuasiQuoter
dedentWith f = quoter $ f . withLines (strip . trim (all isSpace))

withLines :: (Seq String -> Seq String) -> String -> String
withLines f = unsplit '\n' . toList . f . Seq.fromList . split '\n'

split :: Eq a => a -> [a] -> [[a]]
split sep xs = case break (== sep) xs of
  (ys, [])     -> ys : []
  (ys, _ : zs) -> ys : split sep zs

unsplit :: a -> [[a]] -> [a]
unsplit = intercalate . pure

-- | The first line can be safely dropped if it consists only of spaces,
-- but we want to preserve the last newline, thus last line can only be trimmed
trim :: (String -> Bool) -> Seq String -> Seq String
trim f = trimLast f . dropFirst f

dropFirst :: (String -> Bool) -> Seq String -> Seq String
dropFirst p xs = case Seq.viewl xs of
  y :< ys | p y -> ys
  _ -> xs

trimLast :: (String -> Bool) -> Seq String -> Seq String
trimLast p xs = case Seq.viewr xs of
  ys :> y | p y -> ys |> ""
  _ -> xs

strip :: Seq String -> Seq String
strip xs = case Seq.viewr xs of
  vs :> "" -> stripCommonLeadingWhitespace vs |> ""
  _        -> stripCommonLeadingWhitespace xs

stripCommonLeadingWhitespace :: (Functor f, Foldable f) => f String -> f String
stripCommonLeadingWhitespace xs = drop (commonLeadingWhitespace xs) <$> xs

commonLeadingWhitespace :: (Functor f, Foldable f) => f String -> Int
commonLeadingWhitespace = minimumOr 0 . fmap (length . takeWhile isSpace)

minimumOr :: (Foldable f, Ord a) => a -> f a -> a
minimumOr n = maybe n id . foldr (lmin . Just) Nothing
 where
  lmin (Just x) (Just y) = Just (min x y)
  lmin Nothing x = x
  lmin x Nothing = x
