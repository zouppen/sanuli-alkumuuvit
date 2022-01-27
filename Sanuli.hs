module Sanuli where

import Data.Char (isAsciiLower)

-- |Is Sanuli word (containing only a-z and åäö. Drops also words with
-- capital letters since those are abbreviations or names.
isSanuliWord :: String -> Bool
isSanuliWord x = all isSanuliChar x
  where isSanuliChar x = isAsciiLower x || x `elem` "åäö"
