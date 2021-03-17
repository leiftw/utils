module OrdUtil where -- rename?

import Data.List (sortOn,groupBy,nubBy)
import Data.Ord (Down(..),comparing)
import Data.Function (on)
import Data.Foldable (maximumBy)
import Control.Arrow (second)
import qualified Data.Set as S (Set,singleton)

import Safe.Foldable (maximumByMay)

import Util ((.|.))


key :: (x -> k) -> x -> (k,x)
key f v = (f v,v)

best :: (Foldable f,Ord o) => (x -> o) -> f x -> (o,x)
best f = key f . maximumOn f -- another use of `f`
--best f = foldr (max.key f) (0,0) -- `Num/Monoid/Bounded x,o`
--best f = foldr1 (max.key f) -- `x == o`
--best f = maximum . fmap (key f) -- `Functor f`
best' :: (Ord o) => (x -> o) -> [x] -> (o,x)
best' f = head . rank f -- `head . sortOn` lazy enough?

rank :: (Ord o) => (x -> o) -> [x] -> [(o,x)]
rank f = sortOn (Down . fst) . map (key f)
 -- `sortOn` faster than `sortBy . comparing` applying `f` only linear times
 -- but (slower because it) allocates more space, details at:
 -- https://ro-che.info/articles/2016-04-02-descending-sort-haskell


--copied from `Data.List.nubBy` with x<->y, works only one way as it assumes symmetry
dub :: (Ord o) => [o] -> [o]
dub xs = dubInto xs []
 where dubInto [] _ = []
       dubInto (x:xs) ys
        | dubbed x ys = dubInto xs ys
        | otherwise = x : dubInto xs (x:ys)
       dubbed _ [] = False
       dubbed x (y:ys) = x < y || dubbed x ys

-- optimization for things with a totally orderable and a distinguishing feature
dub' :: (Ord o,Eq e,Traversable t) => (x -> o) -> (x -> e) -> t x -> t x
dub' val dis xs = maximumOn val <$> setOn dis xs


-- ON

maximumOn :: (Foldable f,Ord o) => (x -> o) -> f x -> x
maximumOn = maximumBy . comparing -- optimize like `Data.List.sortOn`?
--maximumOn f = foldr1 (maxOn f)

maximayBy :: (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
maximayBy = maximumByMay

maximayOn :: (Foldable f,Ord o) => (x -> o) -> f x -> Maybe x
maximayOn = maximayBy . comparing

maxOn :: (Ord o) => (x -> o) -> x -> x -> x
maxOn f x y | f x > f y = x | otherwise = y


setOn :: (Eq e,Traversable t) => (x -> e) -> t x -> t (S.Set x)
setOn f = fmap S.singleton -- TODO:!

-- bucketOn?

nubOn :: (Eq e) => (x -> e) -> [x] -> [x]
nubOn = nubBy . ((==) `on`) -- optimize like `Data.List.sortOn`?

groupOn :: (Eq e) => (x -> e) -> [x] -> [[x]]
groupOn = groupBy . ((==) `on`) -- (((==EQ) .) .) . comparing

runOn :: (Eq e) => (x -> e) -> [x] -> [x]
runOn = maximumOn length .|. groupOn


insortBy :: (t -> t -> Bool) -> t -> [t] -> [t]
insortBy f x = uncurry (++) . second (x:) . span (`f`x)

insort :: (Ord t) => t -> [t] -> [t]
insort = insortBy (<)
