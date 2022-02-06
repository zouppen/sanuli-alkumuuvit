{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Monad (unless, when)
import Data.List (intercalate, sortOn)
import Data.Maybe (isNothing, isJust)
import qualified Data.Map.Strict as M
import qualified Data.Map as ML
import qualified Data.Set as S
import qualified Data.Text as T
import Options.Applicative
import System.IO
import Text.Printf
import Data.Version
import Paths_sanuli_alkumuuvit

import Kotus (readKotusWordFile)
import Sanuli ( permutateWords, goodWord, isSanuliWord, totalScore
              , totalScoreProjection, sumScore, Score(..))
import WordUtils (toAnagramMap, fromAnagramList, frequency, toFreqList)
import WordPatch (WordPatch(..), readWordPatch)

data Options = Options
  { wordLen     :: Int
  , wordsToFind :: Int
  , wordFile    :: FilePath
  , sanuliPatch :: Maybe FilePath
  , dropLetters :: String
  , quiet       :: Bool
  } deriving (Show)

optParser :: Parser Options
optParser = Options
  <$> option auto ( mempty
                    <> short 'l'
                    <> long "length"
                    <> metavar "INT"
                    <> showDefault
                    <> value 5
                    <> help "Word length"
                  )
  <*> option auto ( mempty
                    <> short 'w'
                    <> long "words"
                    <> metavar "INT"
                    <> showDefault
                    <> value 3
                    <> help "Word group size"
                  )
  <*> strOption ( mempty
                  <> short 'k'
                  <> long "kotus"
                  <> metavar "FILE"
                  <> help "Kotus XML word list file"
                )
  <*> optional ( strOption ( mempty
                             <> short 'p'
                             <> long "patch"
                             <> metavar "FILE"
                             <> help "Custom Sanuli patch file (default: builtin)"
                           ))
  <*> strOption ( mempty
                  <> short 'd'
                  <> long "drop"
                  <> value ""
                  <> metavar "LETTERS"
                  <> help "Drop given letters, for Sanuliketju (default: none)"
                )
  <*> switch ( mempty
               <> short 'q'
               <> long "quiet"
               <> help "Don't print progress"
             )

opts = info (optParser <**> helper)
       ( fullDesc
         <> header ("avausmuuvit - Generate optimal opening moves for Sanuli v" ++ showVersion version)
       )

main = do
  Options{..} <- execParser opts
  let info = unless quiet

  -- Allows printing half lines such as "Waiting... OK"
  hSetBuffering stderr NoBuffering
  
  -- Report what we are going to do
  info $ ePrintf "Word length is %d\n" wordLen
  info $ ePrintf "Word group size is %d\n" wordsToFind

  -- Step 1: Load word list
  info $ ePrintf "Loading Kotus word list... "
  words <- S.fromList <$> readKotusWordFile wordFile
  info $ ePrintf "%d unique words loaded\n" (length words)

  -- Step 2: Patch the word list
  patchedWords <- do
    info $ ePrintf "Loading Sanuli patch file... "
    file <- maybe (getDataFileName "sanuli-patch.yaml") pure sanuliPatch
    WordPatch{..} <- readWordPatch file
    info $ ePrintf "%d additions, %d removals\n" (length addWords) (length dropWords)
    pure $ words `S.difference` dropWords `S.union` addWords

  let
    -- Step 3: Take words of given length
    givenLengthWords = S.filter (\x -> T.length x == wordLen) patchedWords
    -- Step 4: Drop words which have characters not used in Sanuli
    sanuliWords = S.filter isSanuliWord givenLengthWords
    -- Step 5: Calculate frequency map of letters in those words. Drop
    -- letters if asked to do so.
    dropper = flip M.withoutKeys $ S.fromList dropLetters
    freqList = toFreqList $ dropper $ frequency $ concatMap T.unpack $ S.toList sanuliWords
    -- Step 6: Getting the most popular letters from the frequency map
    (keepThese, dropThese) = splitAt (wordsToFind*wordLen) freqList
    ourLetters = S.fromList $ map fst keepThese
    -- Step 7-8: Use the anagram optimization and then pick words
    -- containing only the most popular letters.
    sanuliWordMap = toAnagramMap sanuliWords
    ourWordMap = M.filterWithKey (\k _ -> goodWord wordLen ourLetters k) sanuliWordMap
    -- Step 9: The actual crunching
    solution = permutateWords wordsToFind $ M.toList ourWordMap
    finalSolution = concatMap fromAnagramList solution
    toScore = toScoreLookup sanuliWords

  -- Progress information and statistics. Because the steps above are
  -- lazy, the actual calculation happens when printing or accessing
  -- the information.
  info $ do
    ePrintf "Applying Sanuli word patch... %d words left\n" (length patchedWords)
    ePrintf "Filtering %d-length words... %d words left\n" wordLen (length givenLengthWords)
    ePrintf "Filtering words with Sanuli characters... %d words left\n" (length sanuliWords)
    ePrintf "Calculating frequency map of %d-length words...\n" wordLen 
    ePrintf "Promoted letters:\n"
    putList stderr formatFreq keepThese
    ePrintf "Demoted letters:\n"
    putList stderr formatFreq dropThese
    ePrintf "Number of anagram groups... %d\n" (length sanuliWordMap)
    ePrintf "Number of anagram groups of promoted letters... %d\n" (length ourWordMap)
    ePrintf "Generating CSV... \n\n"

  -- Print results
  putStrLn $ label wordLen wordsToFind
  putList stdout (formatSolution toScore) finalSolution
  
-- |Prints elements in the list line by line, using the given
-- projection function.
putList :: Foldable t => Handle -> (a -> String) -> t a -> IO ()
putList h f = mapM_ (hPutStrLn h . f)

-- |Formats a solution user-friendly
formatSolution :: (T.Text -> Score) -> [T.Text] -> String
formatSolution toScore xs = T.unpack $ T.intercalate "," $ sorted ++ concatMap formatScore scores
  where sorted = sortOn (totalScoreProjection . toScore) xs
        scores = map toScore sorted
        
-- |Formats character frequency user-friendly
formatFreq :: (Char, Int) -> String
formatFreq (char, count) = "  " ++ [char] ++ ": " ++ show count

-- |Formats score to list of two items: [green, yellow]
formatScore :: Score -> [T.Text]
formatScore Score{..} = [T.pack $ show green, T.pack $ show yellow]

-- |Produces a function returning scores for an individual word. The
-- structure is lazy because not all values are required ever (only a
-- fraction of the keys end up being in the result set.
toScoreLookup :: S.Set T.Text -> T.Text -> Score
toScoreLookup words = (ML.!) $ ML.fromSet (totalScore words) words

-- |Pretty ugly label generator.
label :: Int -> Int -> String
label wordLen wordsToFind = intercalate "," names ++ "," ++ intercalate "," scores
  where names = [ "word" ++ show x | x <- [1..wordsToFind] ]
        scores = [ t:show x | x <- [1..wordsToFind], t <- "gy" ]

-- |Helper for printing to stderr
ePrintf :: HPrintfType r => String -> r
ePrintf = hPrintf stderr
