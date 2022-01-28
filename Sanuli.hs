-- |Functions specific for the Sanuli game
module Sanuli where

import Data.Char (isAsciiLower)
import Control.Applicative (Alternative, liftA, empty)
import qualified Data.Set as S
import Control.Monad (foldM)
import Data.Maybe (isJust)
import Combinatorics (kpn)

-- |Is Sanuli word (containing only a-z and åäö. Drops also words with
-- capital letters since those are abbreviations or names.
isSanuliWord :: String -> Bool
isSanuliWord x = all isSanuliChar x
  where isSanuliChar x = isAsciiLower x || x `elem` "åäö"

-- |Return true if the string length has given length and all the
-- letters are in the given set.
goodWord :: Foldable t => Int -> S.Set Char -> t Char -> Bool
goodWord wantedLen set s =
  length s == wantedLen && all (`S.member` set) s

-- |Get permutations of given length which are fine
permutateWords :: Int -> [(String, a)] -> [[(String, a)]]
permutateWords n xs = filter permutationIsFine $ kpn n xs

-- |Return True if the given permutation is OK (no overlapping
-- chars). Sort order makes sure the group is processed only once.
permutationIsFine :: [(String, a)] -> Bool
permutationIsFine xs = isJust $ foldM meld "" $ map fst xs

-- |Meld two ascending lists, returning Nothing if they have common
-- items, and the new item wrapped in Just otherwise.
meld :: (Ord a, Alternative f) => [a] -> [a] -> f [a]
meld a [] = pure a
meld [] b = pure b
meld (a:as) (b:bs) = case compare a b of
  LT -> put a $ meld as (b:bs)
  GT -> put b $ meld (a:as) bs
  EQ -> empty
  where put x = liftA (x:)
