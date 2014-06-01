0.7.1.0
=======

  * Made `dedent` more flexible

0.7.0.0
=======

  * Renamed `spec` interpreter to `examples` to make it `hspec-discover`-compatible

  * Switch API to `Either` from a `Validation`-like thingie

0.6.2.0
=======

  * Added `remake`

  * Generalized `fromErrors`

0.6.1.1
=======

  * Missing test files in cabal tarball

0.6.1.0
=======

  * Added `fromErrors`

0.6.0.0
=======

  * Complete rewrite

  * Added `pretty` and `spec` interpreters

  * Layout nodes can be configured with user/group and file permissions

  * Added symlinks with optionally checked source existence

  * File contents can be specified as raw bytes or text data

  * File can be declared the copy of another file

0.4.0.0
=======

  * Self-balancing `Semigroup`,`Apply`,`Applicative`,`Bind`, and `Monad` instance for `Node`.
    That way we have reasonable auto-derived `Eq` and `Ord` instances

  * Added `name`, `names`, `next`, `node` traversals and fixed `file` and `directory`

  * Added `Layout` construction from existing directories via `fromDirectory`

0.3.1.0
=======

  * Fixed build for GHC 7.4

0.3.0.0
=======

  * Removed parser stuff

  * Added `Default`, `Semigroup`, `Monoid`, and `Applicative` instances for `Layout`

  * Added "extractors" (`System.Directory.Layout.Lens`)

  * Added `Apply` and `Bind` instances for `Layout`

  * Added `Ord` instance for `Layout`

  * Added `Foldable` and `Traversable` instances for `Layout`

  * Rewrote Check and Make (unified errors, simpler implementation)
