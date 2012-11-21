{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Flt is text format for FileLayout data structure, for example
--
-- @
-- c/
-- ..x
-- ..y
-- ....n
-- ....n
-- ....
-- ..z
-- ....t
-- ....t
-- ....
-- d/
-- @
--
-- where '.' stands for space is equivalent of
--
-- @
-- do directory \"c\" $ do
--      file_ \"x\"
--      file \"y\" \"n\nn\n\"
--      file \"z\" \"t\nt\n\"
--    directory_ \"d\"
-- @
--
module Biegunka.FileLayout.Flt
  ( flt, flt'
  ) where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad (guard)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Text.Parsec hiding ((<|>), many)
import           Text.Parsec.Text ()
import           Text.Parsec.Text.Lazy ()

import Biegunka.FileLayout.Internal


-- | Flt data as lazy 'Text' parser
flt ∷ LT.Text → Either String (FL ())
flt = gflt


-- | Flt data as strict 'Text' parser
flt' ∷ T.Text → Either String (FL ())
flt' = gflt


gflt ∷ Stream s Identity Char ⇒ s → Either String (FL ())
gflt = left show . parse (sequence_ <$> many (p_any 0)) "(flt parser)"


p_any ∷ Stream s Identity Char ⇒ Int → Parsec s u (FL ())
p_any n = try (p_directory n) <|> p_file n


p_directory ∷ Stream s Identity Char ⇒ Int → Parsec s u (FL ())
p_directory n = do
  name ← p_directory_name
  inner ← sequence_ <$> try (inners p_any n) <|> return (E ())
  return $ D name inner (E ())


p_file ∷ Stream s Identity Char ⇒ Int → Parsec s u (FL ())
p_file n = do
  name ← p_file_name
  inner ← Just . T.intercalate "\n" <$> try (inners (const p_text) n) <|> return Nothing
  return $ F name inner (E ())


inners ∷ Stream s Identity Char ⇒ (Int → Parsec s u a) → Int → Parsec s u [a]
inners p n = do
  indent ← length <$> many (char ' ')
  guard (indent > n)
  (:) <$> p indent <*> generous_many (string (replicate indent ' ') *> p indent)


generous_many ∷ Parsec s u a → Parsec s u [a]
generous_many p = f
 where
  f = try ((:) <$> p <*> f) <|> return []
{-# INLINE generous_many #-}


p_directory_name ∷ Stream s Identity Char ⇒ Parsec s u String
p_directory_name = some (noneOf "/\n") <* char '/' <* char '\n'


p_file_name ∷ Stream s Identity Char ⇒ Parsec s u String
p_file_name = intercalate "." <$> ((some (noneOf "/.\n") `sepBy` char '.') <* char '\n')


p_text ∷ Stream s Identity Char ⇒ Parsec s u T.Text
p_text = T.pack <$> many (noneOf "\n") <* char '\n'
