-- |Recovers strings from Sanuli wasm file with a brutal parser
module SanuliExtractor where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import Data.Word
import System.IO
import Data.Bits

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
      toEnum <$> foldM trailer (fromIntegral payload) rest

-- |Helper function for UTF-8 parser, processes the multibyte part
trailer :: Alternative f => Int -> Word8 -> f Int
trailer acc x = if (x `testBit` 7) && not (x `testBit` 6)
                then pure $ acc `shiftL` 6 .|. fromIntegral (x .&. 0x3f)
                else empty

parseFile :: A.Parser a -> FilePath -> IO (A.Result a)
parseFile p f = withFile f ReadMode $ \h -> A.parseWith (B.hGetSome h 1024) p B.empty
