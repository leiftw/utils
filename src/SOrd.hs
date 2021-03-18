{-# LANGUAGE FlexibleInstances #-} -- `o,c`?
{-# LANGUAGE UndecidableInstances #-} -- `o,c`?

module SOrd where -- compare `Data.NonEmpty.Class.Sort`!

import Data.List (sort)
import Data.Foldable (toList)

import COrd


-- tries to simplify `Format.FLdbl` variants (and them using things like `MTG.Color.colorder`)

class Sord s where
 sord :: (Foldable f) => f s -> [s]
 sord = toList
 {-# MINIMAL sord #-} -- causes warnings only!

instance {-# OVERLAPS #-} (Ord o) => Sord o where
 sord = sort . toList
-- TODO: collision even with `OVERLAPS`
--instance {-# OVERLAPS #-} (COrd c) => Sord c where
-- sord = cort
