module Main where

import Control.Monad (liftM2)
import GHC.IO (evaluate)
import System.Environment (getArgs)
import Wordlist

main = do
  [wordLenStr, wordFile] <- getArgs
  let wordLen = read wordLenStr
  putStrLn $ "Word length is " ++ show (wordLen :: Int) ++ "."
  
  putStr "Loading word list... "
  words <- loadKotusWords wordFile
  putStrLn $ show (length words) ++ " words loaded"

  let sanuliWords = filter (liftM2 (&&) isSanuliWord (hasLength wordLen)) words
  putStrLn $ "Keeping only Sanuli words... " ++ show (length sanuliWords) ++ " words left"

-- |Just report what we're doing.
say :: String -> IO a -> IO a
say msg act = do
  putStr $ msg ++ "... "
  a <- act
  putStrLn "OK"
  return a
