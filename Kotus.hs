-- |Parser for Institute for the Languages of Finland word list XML
-- file, https://kaino.kotus.fi/sanat/nykysuomi/
module Kotus where

import Text.XML.HXT.Core

-- |Load Kotus words from their XML file as a list of strings. Keeps
-- only the word, ignores declinations and other information.
readKotusWordFile :: String -> IO [String]
readKotusWordFile f = runX loader
  where loader = readDocument [] f >>>
                 deep (isElem >>> hasName "s" >>> getChildren >>> getText)
