{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Text as T
import qualified Data.Set as S
import Data.Yaml (encodeFile)
import SanuliExtractor (parseSanuliWasm)
import Data.Version
import Options.Applicative
import Paths_sanuli_alkumuuvit

import Kotus
import Sanuli (isSanuliWord)
import WordPatch

data Options = Options
  { kotusFile  :: FilePath
  , wasmFile   :: FilePath
  , patchFile  :: FilePath 
  } deriving (Show)

optParser :: Parser Options
optParser = Options
  <$> strOption ( mempty
                  <> short 'k'
                  <> long "kotus"
                  <> metavar "FILE"
                  <> help "Kotus XML word list file"
                )
  <*> strOption ( mempty
                  <> short 'w'
                  <> long "wasm"
                  <> metavar "FILE"
                  <> help "Sanuli .wasm file"
                )
  <*> strOption ( mempty
                  <> short 'p'
                  <> long "patch"
                  <> metavar "FILE"
                  <> help "Write differences to this file"
                )

opts = info (optParser <**> helper)
       ( fullDesc
         <> header ("sanulidiff - Generate difference between Kotus and Sanuli lists v" ++ showVersion version)
       )

main = do
  Options{..} <- execParser opts

  kotusRaw <- S.fromList <$> readKotusWordFile kotusFile
  sanuli <- S.fromList <$> parseSanuliWasm wasmFile

  -- Filter only 5 and 6 lengths which have Sanuli chars
  let kotusCorrectLength = S.filter (\x -> T.length x == 5 || T.length x == 6) kotusRaw
      kotus = S.filter isSanuliWord kotusCorrectLength
      diff = WordPatch { addWords = sanuli `S.difference` kotus
                       , dropWords = kotus `S.difference` sanuli
                       }

  -- Storing
  encodeFile patchFile diff
