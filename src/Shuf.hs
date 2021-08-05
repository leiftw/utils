module Shuf where

import System.Random.Shuffle (shuffle')
import System.Random (mkStdGen)


shuf :: [a] -> Int -> [a]
shuf xs seed = shuffle' xs (length xs) (mkStdGen seed)
