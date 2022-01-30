-- |Parser for Institute for the Languages of Finland word list XML
-- file, https://kaino.kotus.fi/sanat/nykysuomi/
module Kotus where

import Text.XML.HXT.Core
import Data.Set (Set, fromList)

-- |Load Kotus words from their XML file as a set of strings. Keeps
-- only the word, ignores declinations and other information.
readKotusWordFile :: String -> IO (Set String)
readKotusWordFile f = fromList <$> runX loader
  where loader = readDocument [] f >>>
                 deep (isElem >>> hasName "s" >>> getChildren >>> getText)
