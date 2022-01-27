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
import Wordlist

main = do
  [wordLenStr, wordsToFindStr, wordFile] <- getArgs
  let wordLen = read wordLenStr
  let wordsToFind = read wordsToFindStr
  putStrLn $ "Word length is " ++ show wordLen
  putStrLn $ "Word group size is " ++ show wordsToFind

  putStr "Loading word list... "
  words <- loadKotusWords wordFile
  putStrLn $ show (length words) ++ " words loaded"

  let uniqueWords = S.toList $ S.fromList words
      sanuliWords = filter (liftM2 (&&) isSanuliWord (hasLength wordLen)) uniqueWords
      freqList = toFreqList $ frequency $ concat sanuliWords
      (keepThese, dropThese) = splitAt (wordsToFind*wordLen) freqList
      ourLetters = S.fromList $ map fst keepThese
      sanuliWordMap = toWordMap sanuliWords
      ourWordMap = M.filterWithKey (\k _ -> filterPopular wordLen ourLetters k) sanuliWordMap
      solution = permutateWords wordsToFind $ M.toList ourWordMap
      finalSolution = concatMap (rotate . map snd) solution

  putStrLn $ "Taking only unique words..." ++ show (length uniqueWords) ++ " words left"
  putStrLn $ "Keeping only Sanuli words... " ++ show (length sanuliWords) ++ " words left"
  putStrLn $ "Calculating frequency map of " ++ show wordLen ++ "-length words..."
  putStr $ freqShow keepThese
  putStrLn "    -- demotion zone --"
  putStr $ freqShow dropThese
  putStrLn $ "Distinct sequences... " ++ show (M.size sanuliWordMap)
  putStrLn $ "Relevant sequences... " ++ show (M.size ourWordMap)
  putStrLn ""

  mapM_ (putStrLn . formatSolution) finalSolution

formatSolution :: [String] -> String
formatSolution xs = "    " ++ intercalate " " xs

freqShow :: [(Char, Int)] -> String
freqShow list = unlines $ map line list
  where line (char, count) = "    " ++ [char] ++ ": " ++ show count

-- |Return true if the string length has given length and all the
-- letters are in the given set.
filterPopular :: Int -> S.Set Char -> String -> Bool
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

-- |"Rotates" list, e.g. `[["A1","A2"],["B1","B2","B3"]]` ->
-- `[["A1","B1"],["A1","B2"],["A1","B3"],["A2","B1"],["A2","B2"],["A2","B3"]]`
rotate :: [[a]] -> [[a]]
rotate [] = [[]]
rotate (x:xs) = [ a:b | a <- x, b <- rotate xs ]
