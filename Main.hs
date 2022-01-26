module Main where

import Control.Monad (liftM2)
import GHC.IO (evaluate)
import GHC.Exts (sortWith)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Wordlist

main = do
  let wordsToFind = 3
  [wordLenStr, wordFile] <- getArgs
  let wordLen = read wordLenStr
  putStrLn $ "Word length is " ++ show (wordLen :: Int)

  putStr "Loading word list... "
  words <- loadKotusWords wordFile
  putStrLn $ show (length words) ++ " words loaded"

  let sanuliWords = filter (liftM2 (&&) isSanuliWord (hasLength wordLen)) words
      freqList = toFreqList $ frequency $ concat sanuliWords
      (keepThese, dropThese) = splitAt (wordsToFind*wordLen) freqList
      ourLetters = S.fromList $ map fst keepThese
      sanuliWordMap = toWordMap sanuliWords
      ourWordMap = M.filterWithKey (\k _ -> filterPopular wordLen ourLetters k) sanuliWordMap

  putStrLn $
    "Keeping only Sanuli words... " ++ show (length sanuliWords) ++
    " words left"
  putStrLn $ "Calculating frequency map of " ++ show wordLen ++ "-length words..."
  putStr $ freqShow keepThese
  putStrLn "    -- demotion zone --"
  putStr $ freqShow dropThese
  putStrLn $ "Distinct sequences... " ++ show (M.size sanuliWordMap)
  putStrLn $ "Relevant sequences... " ++ show (M.size ourWordMap)

freqShow :: [(Char, Int)] -> String
freqShow list = unlines $ map line list
  where line (char, count) = "    " ++ [char] ++ ": " ++ show count

filterPopular :: Int -> S.Set Char -> String -> Bool
filterPopular wordLen ourLetters distinct =
  length distinct == wordLen && all (`S.member` ourLetters) distinct

{-
-- |Just report what we're doing.
say :: String -> IO a -> IO a
say msg act = do
  putStr $ msg ++ "... "
  a <- act
  putStrLn "OK"
  return a
-}
