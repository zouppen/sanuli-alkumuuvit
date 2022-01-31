-- |Recovers strings from Sanuli wasm file with a brutal parser
module SanuliExtractor where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, Result, anyWord8, word8, many1, sepBy1, parseWith, maybeResult)
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Function (on)
import Data.List (maximumBy)
import System.IO (withFile, IOMode(ReadMode))
import Utf8Parser

-- |Skips characters until parser succeeds
skipGarbage :: Parser a -> Parser a
skipGarbage p = p <|> (anyWord8 >> skipGarbage p)

-- |Go through the file and find all possible Sanuli lists
bestSanuliList :: Parser [T.Text]
bestSanuliList = best <$> many (skipGarbage sanuliList)
  where best = maximumBy (compare `on` length)

-- |List of Sanuli words
sanuliList :: Parser [T.Text]
sanuliList = sanuliString `sepBy1` word8 0x0A

-- |Parses non-empty Sanuli string
sanuliString :: Parser T.Text
sanuliString = many1 sanuliChar >>= validate lenCheck >>= pure . T.toLower . T.pack
  where lenCheck x = length x == 5 || length x == 6

-- |Takes only Sanuli letter
sanuliChar :: Parser Char
sanuliChar = utf8char >>= validate isSanuliLetter 
  where isSanuliLetter = flip S.member $ S.fromList $ ['A'..'Z'] ++ "ÅÄÖ"

-- |Returns the given value only if it passes the check
-- function. Similar to `guard`, but returns the value instead of ().
validate :: Alternative m => (a -> Bool) -> a -> m a
validate f a = if f a
               then pure a
               else empty

-- |Parse Sanuli file and try to dig the words from there
parseSanuliWasm :: FilePath -> IO [T.Text]
parseSanuliWasm file = do
  r <- parseFile bestSanuliList file
  maybe err pure $ maybeResult r
    where err = fail "Unable to parse Sanuli data"

-- |Apply parser to file contents.
parseFile :: Parser a -> FilePath -> IO (Result a)
parseFile p f = withFile f ReadMode $ \h -> parseWith (B.hGetSome h 1024) p B.empty
