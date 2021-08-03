--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-} -- `n` ambiguous
{-# LANGUAGE FlexibleInstances #-} -- `be`
{-# LANGUAGE UndecidableInstances #-} -- `be`

module COrd where

import OrdUtil (insortBy)


class COrd c where --(Num n) => COrd c n
 (-°) :: c -> c -> Int --n
 (<°) :: c -> c -> Bool
 cmin :: c -> c -> c
 cmax :: c -> c -> c
 c0 -° c1
   | c0 <° c1 = -1
   | c1 <° c0 = 1
   | otherwise = 0
 c0 <° c1 = (c0 -° c1) < 0
 cmin c0 c1 = if c0 <° c1 then c0 else c1 -- don't override!
 cmax c0 c1 = if c0 <° c1 then c1 else c0 -- don't override!
 {-# MINIMAL (-°) | (<°) #-}
 -- define `-°` usually, `<°` if distance is meaningless

 -- `a -° a == 0`
 -- `a -° b == 0 <=> a == b` with `Eq`
 -- `a -° b + b -° a == 0`
 -- `a <° a == False`
 -- `a <° b && b <° a <=> a == b` with `Eq`
 -- `<°` NOT necessarily transitive, CAN BE circular!
 -- `c0 <° c1 == (c0 -° c1) < 0`
 -- `{cmin a b,cmax a b} == {a,b}`
 -- [the don't overrides]

 cort :: (Foldable f) => f c -> [c]
 cort = foldr (insortBy (<°)) []


instance {-# OVERLAPPABLE #-} (Enum be,Bounded be) => COrd be where --Int
 (-°) = (|-°|)
-- separate definition so it can be augmented by a `cort` definition
(|-°|) :: (Enum be,Bounded be) => be -> be -> Int
be0 |-°| be1 = ((fromEnum be0 - fromEnum be1) + hlfEnum) `mod` sizEnum - hlfEnum
  where sizEnum = fromEnum (maxBound `asTypeOf` be0) + 1
        hlfEnum = sizEnum `div` 2
 -- alternative to `asTypeOf` would be using `ScopedTypeVariables` with `forall be. `
 -- see thread https://mail.haskell.org/pipermail/haskell-cafe/2008-February/039054.html

-- `FunctionalDependencies` `c -> n` `fromIntegral` fails as well
