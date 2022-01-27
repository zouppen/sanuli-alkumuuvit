{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (Alternative, liftA, empty)
import Control.Monad (foldM, liftM2, replicateM)
import Data.List (intercalate)
import Data.Maybe (isJust)
import GHC.IO (evaluate)
import GHC.Exts (sortWith)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Printf
import Wordlist
import WordPatch
import Sanuli
import Kotus (readKotusWordFile)

main = do
  [wordLenStr, wordsToFindStr, wordFile, sanuliPatch] <- getArgs
  let wordLen = read wordLenStr
  let wordsToFind = read wordsToFindStr
  printf "Word length is %d\n" wordLen
  printf "Word group size is %d\n" wordsToFind

  printf "Loading Kotus word list... "
  words <- readKotusWordFile wordFile
  printf "%d unique words loaded\n" (length words)

  printf "Loading Sanuli patch... "
  WordPatch{..} <- readWordPatch sanuliPatch
  printf "%d additions, %d removals\n" (length addWords) (length dropWords)

  let patchedWords = words `S.difference` dropWords `S.union` addWords
      givenLengthWords = S.filter (hasLength wordLen) patchedWords
      sanuliWords = S.filter isSanuliWord givenLengthWords
      freqList = toFreqList $ frequency $ concat $ S.toList sanuliWords
      (keepThese, dropThese) = splitAt (wordsToFind*wordLen) freqList
      ourLetters = S.fromList $ map fst keepThese
      sanuliWordMap = toAnagramMap sanuliWords
      ourWordMap = M.filterWithKey (\k _ -> filterPopular wordLen ourLetters k) sanuliWordMap
      solution = permutateWords wordsToFind $ M.toList ourWordMap
      finalSolution = concatMap (cartesianProduct . map (S.toList.snd)) solution

  printf "Applying Sanuli word patch... %d words left\n" (length patchedWords)
  printf "Filtering %d-length words... %d words left\n" wordLen (length givenLengthWords)
  printf "Filtering words with Sanuli characters... %d words left\n" (length sanuliWords)
  printf "Calculating frequency map of %d-length words...\n" wordLen 
  printf "Promoted letters:\n"
  putList formatFreq keepThese
  printf "Demoted letters:\n"
  putList formatFreq dropThese
  printf "Number of anagram groups... %d\n" (length sanuliWordMap)
  printf "Number of anagram groups of promoted letters... %d\n" (length ourWordMap)
  printf "Calculating unique word set...\n"

  putList formatSolution finalSolution

-- |Prints elements in the list line by line, using the given
-- projection function.
putList :: Foldable t => (a -> String) -> t a -> IO ()
putList f = mapM_ (putStrLn . f)

-- |Formats a solution user-friendly
formatSolution :: [String] -> String
formatSolution xs = "    " ++ intercalate " " xs

-- |Formats character frequency user-friendly
formatFreq :: (Char, Int) -> String
formatFreq (char, count) = "    " ++ [char] ++ ": " ++ show count

-- |Return true if the string length has given length and all the
-- letters are in the given set.
filterPopular :: Foldable t => Int -> S.Set Char -> t Char -> Bool
filterPopular wantedLen set s =
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

