-- |Parses UTF-8 character with a Bytestring-based Attoparsec
-- parser. This is wider than the original UTF-8 spec which is limited
-- to 4 byte characters. It was easier to make wider than narrower
-- version.
module Utf8Parser (utf8char) where

import Control.Applicative (Alternative, empty)
import Control.Monad (foldM)
import Data.Attoparsec.ByteString (Parser, anyWord8, count)
import Data.Bits
import Data.Word (Word8)

-- Parse UTF-8 character
utf8char :: Parser Char
utf8char = do
  firstByte <- anyWord8
  case countLeadingZeros (complement firstByte) of
    -- Code point below 128
    0 -> pure $ toEnum $ fromIntegral firstByte
    -- Trailing byte is invalid at this stage
    1 -> empty
    -- Multi-byte characte
    n -> do
      let payload = 0xff `shiftR` n .&. firstByte
      rest <- count (n-1) anyWord8
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
