# Directory layout

[![Build Status](https://secure.travis-ci.org/supki/directory-layout.png?branch=master)](http://travis-ci.org/supki/directory-layout)

Directory layout is a specific format for defining directory and files locations.

This library provides some methods for declaring, constructing and verifying directory layouts.

# Usage

Suppose we have simple directory layout:

```
test-dir/
  example-file
    multi
    line
    file
  empty-dir/
```

It could be parsed by `layout` method to `DL()` object:

```haskell
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

It could be rewritten directly with `file` and `directory` methods:

```haskell
l :: DL ()
l = directory "test-dir" $ do
  file "example-file" "multi\nline\nfile"
  directory_ "empty-dir"
```

Now it could be called to apply this directory layout using home directory as a root directory:

```haskell
import System.Directory (getHomeDirectory)
import System.Directory.Layout

main :: IO ()
main = do
  mapM_ print =<< make l =<< getHomeDirectory
```

Existing directory layout could be also verified with `check` method:

```haskell
main :: IO ()
main = do
  mapM_ print =<< check l =<< getHomeDirectory
```