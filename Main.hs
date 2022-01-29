{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Map as ML
import qualified Data.Set as S
import GHC.Exts (sortWith)
import System.Environment (getArgs)
import System.IO
import Text.Printf

import Kotus (readKotusWordFile)
import Sanuli (permutateWords, goodWord, isSanuliWord, totalScore, sumScore, Score(..))
import WordUtils (toAnagramMap, fromAnagramList, frequency, toFreqList)
import WordPatch (WordPatch(..), readWordPatch)

main = do
  hSetBuffering stdout NoBuffering
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
      givenLengthWords = S.filter (\x -> length x == wordLen) patchedWords
      sanuliWords = S.filter isSanuliWord givenLengthWords
      toScore = toScoreLookup sanuliWords
      freqList = toFreqList $ frequency $ concat $ S.toList sanuliWords
      (keepThese, dropThese) = splitAt (wordsToFind*wordLen) freqList
      ourLetters = S.fromList $ map fst keepThese
      sanuliWordMap = toAnagramMap sanuliWords
      ourWordMap = M.filterWithKey (\k _ -> goodWord wordLen ourLetters k) sanuliWordMap
      solution = permutateWords wordsToFind $ M.toList ourWordMap
      finalSolution = concatMap fromAnagramList solution

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
  putList (formatSolution toScore) finalSolution

-- |Prints elements in the list line by line, using the given
-- projection function.
putList :: Foldable t => (a -> String) -> t a -> IO ()
putList f = mapM_ (putStrLn . f)

-- |Formats a solution user-friendly
formatSolution :: (String -> Score) -> [String] -> String
formatSolution toScore xs = "    " ++ intercalate " " sorted ++ " " ++ intercalate " " (map formatScore scores) ++ " = " ++ formatScore combinedScore
  where scores = map toScore sorted
        sorted = reverse $ sortWith toScore xs -- TODO negate projection function
        combinedScore = foldl1 sumScore scores

-- |Formats character frequency user-friendly
formatFreq :: (Char, Int) -> String
formatFreq (char, count) = "    " ++ [char] ++ ": " ++ show count

-- |Formats score like this: greens+yellows
formatScore :: Score -> String
formatScore Score{..} = printf "%4d+%4d" green yellow

-- |Produces a function returning scores for an individual word. The
-- structure is lazy because not all values are required ever (only a
-- fraction of the keys end up being in the result set.
toScoreLookup :: S.Set String -> String -> Score
toScoreLookup words = (ML.!) $ ML.fromSet (totalScore words) words
