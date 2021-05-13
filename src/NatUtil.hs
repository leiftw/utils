module NatUtil where

import Numeric.Natural

import Util ((.|.))


repliconcat :: Natural -> [a] -> [a]
repliconcat = concat .|. replicat

replicat :: Natural -> a -> [a]
replicat = replicate . fromIntegral
