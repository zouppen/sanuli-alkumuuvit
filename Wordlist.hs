module Wordlist where

import GHC.Exts (sortWith)
import Text.XML.HXT.Core
import Data.Char (isAsciiLower)
import Data.List (sort, group)
import qualified Data.Map.Strict as M

type FreqMap = M.Map Char Int
type WordMap = M.Map String [String]

-- |Load Kotus words from their XML file as a list of strings.
loadKotusWords :: String -> IO [String]
loadKotusWords f = runX loader
  where loader = readDocument [] f >>>
                 deep (isElem >>> hasName "s" >>> getChildren >>> getText)

-- |Generate a reverse map of words
toWordMap :: [String] -> WordMap
toWordMap words = project toDistinct words

-- |Convert word to sorted distinct letter sequence
toDistinct :: String -> String
toDistinct = map head . group . sort

-- |Has specific length
hasLength :: Int -> String -> Bool
hasLength n xs = length xs == n

-- |Is Sanuli word (containing only a-z and åäö. Drops also words with
-- capital letters since those are abbreviations or names.
isSanuliWord :: String -> Bool
isSanuliWord x = all isSanuliChar x
  where isSanuliChar x = isAsciiLower x || x `elem` "åäö"

-- |Letter frequency of words.
frequency :: String -> FreqMap
frequency words = M.fromListWith (+) $ map toFreq words
  where toFreq a = (a,1)

-- |Letter frequencies in descending order.
toFreqList :: FreqMap -> [(Char, Int)]
toFreqList = sortWith (negate . snd) . M.toList

-- |Calculates frequency point for the word. Just adds frequencies of
-- each letter.
points :: FreqMap -> String -> Int
points m xs = sum $ map (m M.!) xs

-- |Insert the values into a map, using projection function as a
-- key. The function doesn't need to be injection. The returned map
-- key is the value of the projection function and the value is a list
-- of values having that projection.
project :: (Foldable t, Ord k) => (a -> k) -> t a -> M.Map k [a]
project f xs = foldl inserter M.empty xs
  where
    -- Function which inserts values to the map
    inserter m x = M.alter (alternator x) (f x) m
    -- Adds item to the list if any, otherwise create singleton list
    alternator x = Just . maybe [x] (x:)
