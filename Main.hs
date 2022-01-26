module Main where

import Control.Monad (liftM2)
import GHC.IO (evaluate)
import GHC.Exts (sortWith)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
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
      ourLetters = map fst $ take (wordsToFind*wordLen) freqList

  putStrLn $
    "Keeping only Sanuli words... " ++ show (length sanuliWords) ++
    " words left"
  putStr $
    "Calculating frequency map of " ++ show wordLen ++
    "-length words...\n" ++ freqShow freqList
  putStrLn $ "Our letters of interest: " ++ ourLetters

freqShow :: [(Char, Int)] -> String
freqShow list = unlines $ map line list
  where line (char, count) = "    " ++ [char] ++ ": " ++ show count

{-
-- |Just report what we're doing.
say :: String -> IO a -> IO a
say msg act = do
  putStr $ msg ++ "... "
  a <- act
  putStrLn "OK"
  return a
-}
