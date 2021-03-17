module Chain where


chain :: [t] -> [(t,t)]
chain (a:b:r) = (a,b) : chain(b:r)
chain _ = []

naiveUnchain :: [(t,t)] -> [t]
naiveUnchain ((a,b):r) = a : b : map snd r

unchain :: (Eq e) => [(e,e)] -> [e]
unchain ((a,b):r) = a: unchainer b r

unchainer :: (Eq e) => e -> [(e,e)] -> [e]
unchainer b' ((b,c):r) | b' == b   = b: unchainer c r
                       | otherwise = []

strictUnchain :: (Eq e) => [(e,e)] -> Maybe [e]
strictUnchain ((a,b):r) = (a:) <$> strictUnchainer b r
 -- taking away `Maybe` by:
 -- `fromJust` causes unexplained error
 -- `concat` is hacky and arbitrary in that the chain could perhaps go further than one element

strictUnchainer :: (Eq e) => e -> [(e,e)] -> Maybe [e]
strictUnchainer b' ((b,c):r) | b' == b   = (b:) <$> strictUnchainer c r
                             | otherwise = Nothing
