-- |Recovers strings from Sanuli wasm file with a brutal parser
module SanuliExtractor where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Function (on)
import Data.List
import Data.Word
import System.IO
import Data.Bits

-- |Skips characters until parser succeeds
skipGarbage :: A.Parser a -> A.Parser a
skipGarbage p = p <|> (A.anyWord8 >> skipGarbage p)

-- |Go through the file and find all possible Sanuli lists
bestSanuliList :: A.Parser [T.Text]
bestSanuliList = best <$> many (skipGarbage sanuliList)
  where best = maximumBy (compare `on` length)

-- |List of Sanuli words
sanuliList :: A.Parser [T.Text]
sanuliList = sanuliString `A.sepBy1` (A.word8 0x0A)

-- |Parses non-empty Sanuli string
sanuliString :: A.Parser T.Text
sanuliString = T.pack <$> (A.many1 sanuliChar >>= aGuard lenCheck)
  where lenCheck x = length x == 5 || length x == 6

-- |Takes only Sanuli letter
sanuliChar :: A.Parser Char
sanuliChar = utf8char >>= aGuard isSanuliLetter 
  where isSanuliLetter = flip S.member $ S.fromList $ ['A'..'Z'] ++ "ÅÄÖ"

aGuard :: Alternative m => (a -> Bool) -> a -> m a
aGuard f a = if f a
             then pure a
             else empty

-- Parse UTF-8 character
utf8char :: A.Parser Char
utf8char = do
  firstByte <- A.anyWord8
  case countLeadingZeros (complement firstByte) of
    -- Code point below 128
    0 -> pure $ toEnum $ fromIntegral firstByte
    -- Trailing byte is invalid at this stage
    1 -> empty
    -- Multi-byte characte
    n -> do
      let payload = 0xff `shiftR` n .&. firstByte
      rest <- A.count (n-1) A.anyWord8
      foldM trailer (fromIntegral payload) rest >>= tryToChar

-- |Tries conversion to char type. Fails if too big.
tryToChar :: Alternative f => Int -> f Char
tryToChar x = if x > fromEnum (maxBound :: Char)
              then empty
              else pure $ toEnum x

-- |Helper function for UTF-8 parser, processes the multibyte part
trailer :: Alternative f => Int -> Word8 -> f Int
trailer acc x = if (x `testBit` 7) && not (x `testBit` 6)
                then pure $ acc `shiftL` 6 .|. fromIntegral (x .&. 0x3f)
                else empty

-- |Apply parser to file contents.
parseFile :: A.Parser a -> FilePath -> IO (A.Result a)
parseFile p f = withFile f ReadMode $ \h -> A.parseWith (B.hGetSome h 1024) p B.empty
