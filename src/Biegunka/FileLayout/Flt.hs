{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.FileLayout.Flt
  ( flt, flt'
  ) where

import Control.Applicative
import Control.Arrow (left)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Text.Parsec hiding ((<|>), many)
import           Text.Parsec.Text ()
import           Text.Parsec.Text.Lazy ()

import Biegunka.FileLayout.Internal


-- | .flt files as lazy 'Text' parser
flt ∷ LT.Text → Either String (FL ())
flt = gflt


-- | .flt files as strict 'Text' parser
flt' ∷ T.Text → Either String (FL ())
flt' = gflt


gflt ∷ Stream t Identity Char ⇒ t → Either String (FL ())
gflt = left show . parse (sequence_ <$> many (p_any 0)) "(flt parser)"


p_any ∷ Stream t Identity Char ⇒ Int → Parsec t u (FL ())
p_any n = try (p_directory n) <|> p_empty_file


p_directory ∷ Stream t Identity Char ⇒ Int → Parsec t u (FL ())
p_directory n = do
  name ← p_directory_name
  indent ← length <$> lookAhead (many (char ' '))
  (\inner → D name inner (E ())) <$>
    if (indent > n)
      then sequence_ <$>
        generous_some (string (replicate indent ' ') *> p_any indent)
      else return $ E ()


generous_some ∷ Parsec t u a → Parsec t u [a]
generous_some p = (:) <$> p <*> f
 where
  f = try ((:) <$> p <*> f) <|> return []


{-
p_file ∷ Parsec String Int (FL ())
p_file = do
  name ← p_file_name
  --contents ← p_text
  next ← p_any
  --return (F name (Just $ T.pack contents) next)
  return (F name Nothing next)
-}


p_empty_file ∷ Stream t Identity Char ⇒ Parsec t u (FL ())
p_empty_file = do
  name ← p_file_name
  return (F name Nothing (E ()))


{-
p_text = many1 anyChar
-}


p_directory_name ∷ Stream t Identity Char ⇒ Parsec t u String
p_directory_name = some (noneOf "/\n") <* char '/' <* char '\n'


p_file_name ∷ Stream t Identity Char ⇒ Parsec t u String
p_file_name = intercalate "." <$> ((some (noneOf "/.\n") `sepBy` char '.') <* char '\n')
