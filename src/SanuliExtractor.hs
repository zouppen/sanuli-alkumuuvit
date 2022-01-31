-- |Recovers strings from Sanuli wasm file with a brutal parser
module SanuliExtractor where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Function (on)
import Data.List
import System.IO
import Utf8Parser

-- |Skips characters until parser succeeds
skipGarbage :: A.Parser a -> A.Parser a
skipGarbage p = p <|> (A.anyWord8 >> skipGarbage p)

-- |Go through the file and find all possible Sanuli lists
bestSanuliList :: A.Parser [T.Text]
bestSanuliList = best <$> many (skipGarbage sanuliList)
  where best = maximumBy (compare `on` length)

-- |List of Sanuli words
sanuliList :: A.Parser [T.Text]
sanuliList = sanuliString `A.sepBy1` A.word8 0x0A

-- |Parses non-empty Sanuli string
sanuliString :: A.Parser T.Text
sanuliString = A.many1 sanuliChar >>= validate lenCheck >>= pure . T.pack
  where lenCheck x = length x == 5 || length x == 6

-- |Takes only Sanuli letter
sanuliChar :: A.Parser Char
sanuliChar = utf8char >>= validate isSanuliLetter 
  where isSanuliLetter = flip S.member $ S.fromList $ ['A'..'Z'] ++ "ÅÄÖ"

-- |Returns the given value only if it passes the check
-- function. Similar to `guard`, but returns the value instead of ().
validate :: Alternative m => (a -> Bool) -> a -> m a
validate f a = if f a
               then pure a
               else empty

-- |Apply parser to file contents.
parseFile :: A.Parser a -> FilePath -> IO (A.Result a)
parseFile p f = withFile f ReadMode $ \h -> A.parseWith (B.hGetSome h 1024) p B.empty
