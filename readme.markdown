# Directory layout [![Build Status](https://secure.travis-ci.org/supki/directory-layout.png?branch=master)](http://travis-ci.org/supki/directory-layout)

## About 
Directory layout is a specific format of defining directories and files locations.  
This library provides functions for declaring, constructing and verifying directory layouts.

## Usage

Suppose we have a simple directory layout:

```
test-dir/
  example-file
    multi
    line
    file
  empty-dir/
```

`layout` function converts text representation into `DL ()` value:

```haskell
import System.Directory.Layout
import qualified Data.Text.Lazy as T

ltext :: Either String (DL ())
ltext = layout $ T.unlines
  [ "test-dir/"
  , "  example-file"
  , "    multi"
  , "    line"
  , "    file"
  , "  empty-dir/"
  ]
```

Or this `DL ()` value might be written directly:

```haskell
l :: DL ()
l = directory "test-dir" $ do
  file "example-file" "multi\nline\nfile"
  directory_ "empty-dir"
```

Then we could make layout specified by `l` with home directory as root and print warnings (if any):

```haskell
import System.Directory (getHomeDirectory)

main :: IO ()
main = mapM_ print =<< make l =<< getHomeDirectory
```

Also we could verify that `l` corresponds to existing directory layout with `check` function and print failures (if any):

```haskell
main :: IO ()
main = mapM_ print =<< check l =<< getHomeDirectory
```