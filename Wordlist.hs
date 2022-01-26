module Wordlist where

import GHC.Exts (sortWith)
import Text.XML.HXT.Core
import Data.Char (isAsciiLower)
import Data.List (sort, group)
import qualified Data.Map.Strict as M
--import qualified Data.Set.Strict as S

type FreqMap = M.Map Char Int
type WordMap = M.Map String Word

data WordInfo = WordInfo
  { letters      :: String -- ^Sorted list of unique letters
  , allDifferent :: Bool   -- ^Are letters all different
  } deriving (Show)

-- |Load Kotus words from their XML file as a list of strings.
loadKotusWords :: String -> IO [String]
loadKotusWords f = runX loader
  where loader = readDocument [] f >>>
                 deep (isElem >>> hasName "s" >>> getChildren >>> getText)

-- |Generate a map of words
toWordMap :: [String] -> M.Map String WordInfo
toWordMap list = M.fromList $ map (\word -> (word, toWordInfo word)) list

-- |Get information about the word.
toWordInfo :: String -> WordInfo
toWordInfo word = WordInfo (map head uniq) (length word == length uniq)
  where uniq = group $ sort word

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
