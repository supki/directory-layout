{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Exception (ErrorCall(..), throwIO)
import Data.Monoid ((<>))

import           System.Directory.Layout
import qualified Data.Text.Lazy.IO as T
import           Options.Applicative


data Mode = Check | Make
data Option = Option
  { layout_path ∷ FilePath
  , mode ∷ Mode
  , root_path ∷ FilePath
  }


main ∷ IO ()
main =
  execParser parser >>= \Option {layout_path, mode, root_path} → do
    layout <$> T.readFile layout_path >>= either (throwIO . ErrorCall) (act mode root_path) >>= \es →
      case es of
        [] → putStrLn "Done. No errors."
        _  → mapM_ putStrLn ("Done. Errors encountered:":es)
 where
  act mode path fl = case mode of
    Check → map show <$> check fl path
    Make → map show <$> make fl path


parser ∷ ParserInfo Option
parser = info (helper <*> opts) $
  fullDesc <> progDesc "Check or make a layout specified" <> header "directory-layout cli"
 where
  opts = Option <$>
    strOption (long "layout" <> short 'l' <> metavar "FILE" <> help "Layout file path") <*>
    flag Check Make (long "make" <> help "Make layout, don't check") <*>
    argument str (metavar "PATH")
