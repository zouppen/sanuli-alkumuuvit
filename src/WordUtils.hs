-- |Somewhat generic functions for many kinds of word games.
module WordUtils where

import Data.List (sort, sortOn, group)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, unpack)
import Combinatorics (cartesianProduct, project)

type Letters  = String                 -- ^Set of letters the word consists
type Anagrams = S.Set Text             -- ^Set of words which are anagrams
type FreqMap  = M.Map Char Int         -- ^Character frequency map
type WordMap  = M.Map Letters Anagrams -- ^Map of all words (grouped by anagrams)

-- |Group words into Map of anagrams. Anagrams are useful in
-- optimizing the search space since it allows the words with the same
-- letters to processed as one and after the result is found, then
-- converted back to words.
toAnagramMap :: Foldable t => t Text -> WordMap
toAnagramMap words = project toLetters words

-- |Convert word to sorted distinct letter sequence
toLetters :: Text -> Letters
toLetters = map head . group . sort . unpack

-- |Letter frequency of words.
frequency :: String -> FreqMap
frequency words = M.fromListWith (+) $ map toFreq words
  where toFreq a = (a,1)

-- |Letter frequencies in descending order.
toFreqList :: FreqMap -> [(Char, Int)]
toFreqList = sortOn (negate . snd) . M.toList

-- |Calculates frequency point for the word. Just adds frequencies of
-- each letter.
points :: FreqMap -> String -> Int
points m xs = sum $ map (m M.!) xs

-- |Extracts all possible strings from an anagram entry.
fromAnagrams :: (a, Anagrams) -> [Text]
fromAnagrams = S.toList . snd

-- |When given list of anagrams, outputs cartesian product of them,
-- resulting all possible combinations of such words. So, it unfolds
-- the list of anagrams to real words.
fromAnagramList :: [(a, Anagrams)] -> [[Text]]
fromAnagramList = cartesianProduct . map fromAnagrams
