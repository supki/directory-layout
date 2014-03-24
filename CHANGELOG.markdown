0.5.0.0
=======

  * simplify `make` interface: it always creates the layout in the current directory

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
