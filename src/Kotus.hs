-- |Parser for Institute for the Languages of Finland word list XML
-- file, https://kaino.kotus.fi/sanat/nykysuomi/
module Kotus where

import Data.Text (Text, pack)
import Text.XML.HXT.Core

-- |Load Kotus words from their XML file as a set of strings. Keeps
-- only the word, ignores declinations and other information.
readKotusWordFile :: FilePath -> IO [Text]
readKotusWordFile f = map pack <$> runX loader
  where loader = readDocument [] f >>>
                 deep (isElem >>> hasName "s" >>> getChildren >>> getText)
