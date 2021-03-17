{-# LANGUAGE OverloadedLists #-}

module SUtil where -- (module Data.Set,module SUtil)

import Prelude hiding (map)
import Data.Set hiding (foldr)
import Data.Maybe (mapMaybe)

import Util ((.|.))


-- clashes with the one for lists from `Util`
n_one :: (Ord o) => Set o -> Set (Maybe o)
n_one xs = insert Nothing (map Just xs)


-- TODO: cheat to get instances for `Set`
{-
instance Functor Set where
 fmap = map

instance Applicative Set where
 --pure :: t -> Set t
 pure = singleton
 --(<*>) :: Set (t0 -> t1) -> t0 -> t1
 gs <*> xs = foldr' (union . (flip map xs)) [] gs
-}


-- ´<>` instead of `union` could be confusing with contained `Monoid`

seqfold1Set :: (Foldable f,Ord o) => (o -> o -> o) -> f (Set o) -> Set o
seqfold1Set b = foldr1 (\xs -> foldr (union . (\y -> map (b y) xs)) [])

--seqfoldSet :: (Foldable f,Ord o,Ord oa) => (o -> oa -> oa) -> Set oa -> f (Set o) -> Set oa
-- inferred: (Foldable f,Foldable f',Ord o) => (t -> oa -> oa) -> Set oa -> f (f' t) -> Set oa
seqfoldSet b = foldr (flip (\xs -> foldr (union . (\y -> map (b y) xs)) [])) -- hacky!
-- probably want: (Foldable f,Ord o) => (t -> o -> Set o) -> Set o -> f t -> Set o
--seqfoldSet f = foldr (union . (\y -> map (f y) xs) [])
--seqfoldSet f = foldr (\y -> flattunion . map (f y)) -- (flattunion .|. map . f) -- flattunion :: Set (Set t) -> Set t
--seqfoldSet f = foldr (\y -> foldr (union . map (f y)) [])

-- currently unused, not `Set`-specific
seqfoldoid :: (Foldable f,Ord o,Foldable m,Functor m,Monoid (m o)) => (o -> o -> o) -> f (m o) -> m o
seqfoldoid b = foldr1 (\xs -> foldr ((<>) . (\y -> fmap (b y) xs)) mempty)
-- how to do without `Functor`? -- isn't `Foldable Functor` `Traversable`?


sequenset :: (Traversable t,Ord o) => t (Set o) -> Set (t o)
sequenset = undefined -- TODO: implement!

-- suggested in https://github.com/haskell/containers/issues/346
-- and first in https://gitlab.haskell.org/ghc/ghc/issues/12828 , and then
-- https://mail.haskell.org/pipermail/libraries/2016-November/thread.html#27453 (continued December)
-- https://www.reddit.com/r/haskell/comments/2y2pe5/shouldnt_ftp_propagate_changes_over_the_entire/
smapMaybe :: (Ord o0,Ord o1) => (o0 -> Maybe o1) -> Set o0 -> Set o1
smapMaybe f = fromList . mapMaybe f . toList
-- = map fromJust . delete Nothing . map f
