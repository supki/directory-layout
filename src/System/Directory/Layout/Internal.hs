{-# LANGUAGE TypeFamilies #-}
-- | Free monad based directory layouts
module System.Directory.Layout.Internal
  ( Node(..), Layout
  ) where

import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)


-- | Type synonym to save some acrobatics
type Layout = Node ()


-- | The representation of directory layouts
data Node a =
    E a                              -- ^ emptyness
  | F FilePath (Maybe Text) (Node a) -- ^ file name and contents
  | L FilePath String (Node a)       -- ^ link name and the source
  | D FilePath Layout (Node a)       -- ^ directory name and contents
    deriving (Show, Read, Eq, Ord)

compareFilePath :: Node a -> Node b -> Ordering
compareFilePath (E {})     (E {})      = EQ
compareFilePath (E {})     _           = LT
compareFilePath _          (E {})      = GT
compareFilePath (L fp _ _) (L fp' _ _) = compare fp fp'
compareFilePath (L {})     _           = LT
compareFilePath _          (L {})      = GT
compareFilePath (F fp _ _) (F fp' _ _) = compare fp fp'
compareFilePath (F {})     _           = LT
compareFilePath _          (F {})      = GT
compareFilePath (D fp _ _) (D fp' _ _) = compare fp fp'
{-# INLINE compareFilePath #-}

instance Semigroup (Node a) where
  (<>) = (>>)
  {-# INLINE (<>) #-}

instance () ~ a => Monoid (Node a) where
  mempty = return ()
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Functor Node where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Apply Node where
  f <.> x =
    f >>- \f' ->
    x >>- \x' ->
    pure (f' x')
  {-# INLINE (<.>) #-}

instance Applicative Node where
  pure = E
  {-# INLINE pure #-}

  (<*>) = (<.>)
  {-# INLINE (<*>) #-}

instance Bind Node where
  E x         >>- f = f x
  n@(L _ _ x) >>- f = n >>* (x >>- f)
  n@(F _ _ x) >>- f = n >>* (x >>- f)
  n@(D _ _ x) >>- f = n >>* (x >>- f)
  {-# INLINE (>>-) #-}

(>>*) :: Node a -> Node b -> Node b
a >>* b =
  case compareFilePath a b of
    GT -> case b of
      E x      -> x <$ a
      L f s l  -> L f s (a >>* l)
      F f t l  -> F f t (a >>* l)
      D f l l' -> D f l (a >>* l')
    _  -> case a of
      E _     -> b
      L f s _ -> L f s b
      F f t _ -> F f t b
      D f l _ -> D f l b
{-# INLINE (>>*) #-}

-- | All the crazy stuff here (and in @(>>-)@) is only to get do-notation essentially.
instance Monad Node where
  return = pure
  {-# INLINE return #-}

  a >> b =
    case compareFilePath a b of
      GT -> case b of
        E x      -> x <$ a
        L f s l  -> L f s (a >> l)
        F f t l  -> F f t (a >> l)
        D f l l' -> D f l (a >> l')
      _  -> case a of
        E _      -> b
        L f s l  -> L f s (l  >> b)
        F f t l  -> F f t (l  >> b)
        D f l l' -> D f l (l' >> b)
  {-# INLINE (>>) #-}

  (>>=) = (>>-)
  {-# INLINE (>>=) #-}

instance Foldable Node where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable Node where
  traverse f (E x)      = E      <$> f x
  traverse f (L fp s x) = L fp s <$> traverse f x
  traverse f (F fp t x) = F fp t <$> traverse f x
  traverse f (D fp x y) = D fp x <$> traverse f y
  {-# INLINE traverse #-}
