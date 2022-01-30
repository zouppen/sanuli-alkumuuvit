{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (unless, when)
import Data.List (intercalate, sortOn)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M
import qualified Data.Map as ML
import qualified Data.Set as S
import Options.Applicative
import System.IO
import Text.Printf

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
  , outFile     :: Maybe FilePath
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
                  <> help "Kotus XML file"
                )
  <*> optional ( strOption ( mempty
                             <> short 'p'
                             <> long "patch"
                             <> metavar "FILE"
                             <> help "Sanuli patch file"
                           ))
  <*> optional ( strOption ( mempty
                             <> short 'o'
                             <> long "out"
                             <> metavar "FILE"
                             <> help "Produce CSV file (instead of stdout)"
                           ))
  <*> switch ( mempty
               <> short 'q'
               <> long "quiet"
               <> help "Don't print progress"
             )

opts = info (optParser <**> helper)
       ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "avausmuuvit - Generate optimal opening moves for Sanuli"
       )

main = do
  Options{..} <- execParser opts
  let info = unless quiet

  -- Allows printing half lines such as "Waiting... OK"
  hSetBuffering stdout NoBuffering
  
  -- Just say what we're going to do
  info $ printf "Word length is %d\n" wordLen
  info $ printf "Word group size is %d\n" wordsToFind

  info $ printf "Loading Kotus word list... "
  words <- readKotusWordFile wordFile
  info $ printf "%d unique words loaded\n" (length words)

  patchedWords <- case sanuliPatch of
    Just file -> do
      info $ printf "Loading Sanuli patch... "
      WordPatch{..} <- readWordPatch file
      info $ printf "%d additions, %d removals\n" (length addWords) (length dropWords)
      pure $ words `S.difference` dropWords `S.union` addWords
    Nothing -> pure words

  let givenLengthWords = S.filter (\x -> length x == wordLen) patchedWords
      sanuliWords = S.filter isSanuliWord givenLengthWords
      toScore = toScoreLookup sanuliWords
      freqList = toFreqList $ frequency $ concat $ S.toList sanuliWords
      (keepThese, dropThese) = splitAt (wordsToFind*wordLen) freqList
      ourLetters = S.fromList $ map fst keepThese
      sanuliWordMap = toAnagramMap sanuliWords
      ourWordMap = M.filterWithKey (\k _ -> goodWord wordLen ourLetters k) sanuliWordMap
      solution = permutateWords wordsToFind $ M.toList ourWordMap
      finalSolution = concatMap fromAnagramList solution

  -- Progress information and statistics
  info $ do
    printf "Applying Sanuli word patch... %d words left\n" (length patchedWords)
    printf "Filtering %d-length words... %d words left\n" wordLen (length givenLengthWords)
    printf "Filtering words with Sanuli characters... %d words left\n" (length sanuliWords)
    printf "Calculating frequency map of %d-length words...\n" wordLen 
    printf "Promoted letters:\n"
    putList stdout formatFreq keepThese
    printf "Demoted letters:\n"
    putList stdout formatFreq dropThese
    printf "Number of anagram groups... %d\n" (length sanuliWordMap)
    printf "Number of anagram groups of promoted letters... %d\n" (length ourWordMap)
    printf "Generating CSV... "
    when (isNothing outFile) $ putStr "\n\n"

  -- Actual results
  h <- maybe (pure stdout) (flip openFile WriteMode) outFile
  hPutStr h $ label wordLen wordsToFind
  putList h (formatSolution toScore) finalSolution
  unless (isNothing outFile) $ do
    hClose h
    info $ putStrLn " OK"
  
-- |Prints elements in the list line by line, using the given
-- projection function.
putList :: Foldable t => Handle -> (a -> String) -> t a -> IO ()
putList h f = mapM_ (hPutStrLn h . f)

-- |Formats a solution user-friendly
formatSolution :: (String -> Score) -> [String] -> String
formatSolution toScore xs = intercalate "," $ sorted ++ map formatScore scores
  where sorted = sortOn (totalScoreProjection . toScore) xs
        scores = map toScore sorted
        
-- |Formats character frequency user-friendly
formatFreq :: (Char, Int) -> String
formatFreq (char, count) = "  " ++ [char] ++ ": " ++ show count

-- |Formats score like this: greens+yellows
formatScore :: Score -> String
formatScore Score{..} = show green ++ "," ++ show yellow

-- |Produces a function returning scores for an individual word. The
-- structure is lazy because not all values are required ever (only a
-- fraction of the keys end up being in the result set.
toScoreLookup :: S.Set String -> String -> Score
toScoreLookup words = (ML.!) $ ML.fromSet (totalScore words) words

-- |Pretty ugly label generator.
label :: Int -> Int -> String
label wordLen wordsToFind = intercalate "," names ++ "," ++ intercalate "," scores
  where names = [ "word" ++ show x | x <- [1..wordsToFind] ]
        scores = [ t:show x | x <- [1..wordsToFind], t <- "gy" ]
