{-# LANGUAGE RecordWildCards #-}
-- |Functions specific for the Sanuli game. See WordUtils.hs for more
-- generic ones.
module Sanuli where

import Data.Char (isAsciiLower)
import Data.List (sort)
import qualified Data.Text as T
import Control.Applicative (Alternative, liftA, empty)
import qualified Data.Set as S
import Control.Monad (foldM)
import Data.Maybe (isJust)
import Combinatorics (kpn)

data Score = Score { green  :: Int
                   , yellow :: Int
                   } deriving (Show)

-- |Is Sanuli word (containing only a-z and åäö. Drops also words with
-- capital letters since those are abbreviations or names.
isSanuliWord :: T.Text -> Bool
isSanuliWord x = T.all isSanuliChar x
  where isSanuliChar x = isAsciiLower x || x `elem` "åäö"

-- |Return true if the string length has given length and all the
-- letters are in the given set.
goodWord :: Foldable t => Int -> S.Set Char -> t Char -> Bool
goodWord wantedLen set s =
  length s == wantedLen && all (`S.member` set) s

-- |Get permutations of given length which are fine
permutateWords :: Int -> [(String, a)] -> [[(String, a)]]
permutateWords n xs = filter f $ kpn n xs
  where f = permutationIsFine . map fst

-- |Return True if the given permutation is OK (no overlapping
-- chars). Sort order makes sure the group is processed only once.
permutationIsFine :: [String] -> Bool
permutationIsFine = isJust . foldM meld ""

-- |Meld two ascending lists, returning empty if they have common
-- items, and the new item (wrapped in pure) otherwise.
meld :: (Ord a, Alternative f) => [a] -> [a] -> f [a]
meld a [] = pure a
meld [] b = pure b
meld (a:as) (b:bs) = case compare a b of
  LT -> put a $ meld as (b:bs)
  GT -> put b $ meld (a:as) bs
  EQ -> empty
  where put x = liftA (x:)

-- |Find common items from two ascending lists. a is the item to test,
-- b is the reference word.
common :: String -> String -> Int
common _ [] = 0
common [] _ = 0
common (a:as) (b:bs) = case compare a b of
  LT -> common as (b:bs)
  GT -> common (a:as) bs
  EQ -> 1 + common as bs

-- |Calculates score of a word with a reference word.
singleWordScore :: String -> String -> Score
singleWordScore a ref = Score{..}
  where yellow = common (sort a) (sort ref) - green
        green = trues $ zipWith (==) a ref
        trues = length . filter id

-- |Calculates the total score i.e. sum of scores of against the whole
-- word bank.
totalScore :: Foldable t => t T.Text -> T.Text -> Score
totalScore words word = foldl f (Score 0 0) words
  where f acc ref = sumScore acc $ singleWordScore (T.unpack word) (T.unpack ref)

-- |Sums two scores
sumScore :: Score -> Score -> Score
sumScore a b = Score { green = green a + green b
                     , yellow = yellow a + yellow b
                     }

-- |Priorize using total number, but green as a tiebreaker
totalScoreProjection :: Score -> (Int, Int)
totalScoreProjection Score{..} = (-green-yellow, -green)
