module FoldUtil where

import Control.Applicative (Applicative,liftA2)


foldApp :: (Foldable f,Applicative a) => (t -> t -> t) -> f (a t) -> a t
foldApp b = foldr1 (liftA2 b)

-- currently unused
seqfoldoid :: (Foldable f,Foldable m,Functor m,Monoid (m t)) => (t -> t -> t) -> f (m t) -> m t
seqfoldoid b = foldr1 (\xs -> foldr ((<>) . (\y -> fmap (b y) xs)) mempty)
-- how to do without `Functor`? -- isn't `Foldable Functor` `Traversable`?
