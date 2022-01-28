-- |Some generic math / permutation functions
module Combinatorics where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- |Produces all k-permutations of n. Don't ask, don't tell is the
-- documentation of what happens here.
kpn :: Int -> [a] -> [[a]]
kpn 0 _ = [[]]
kpn n [] = []
kpn n (a:xs) = [ a:b | b <- kpn (n-1) xs ] ++ kpn n xs

-- |Calculates n-dimensional cartesian product of the input list
-- e.g. `[["A1","A2"],["B1","B2","B3"]]` ->
-- `[["A1","B1"],["A1","B2"],["A1","B3"],["A2","B1"],["A2","B2"],["A2","B3"]]`
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (x:xs) = [ a:b | a <- x, b <- cartesianProduct xs ]

-- |Insert the values into a map, using projection function as a
-- key. The function doesn't need to be injection. The returned map
-- key is the value of the projection function and the value is a set
-- of values having that projection.
project :: (Foldable t, Ord k, Ord a) => (a -> k) -> t a -> M.Map k (S.Set a)
project f xs = foldl inserter M.empty xs
  where
    -- Function which inserts values to the map
    inserter m x = M.alter (alternator x) (f x) m
    -- Adds item to the list if any, otherwise create singleton list
    alternator x = Just . maybe (S.singleton x) (S.insert x)
