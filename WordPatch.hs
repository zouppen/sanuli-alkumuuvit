{-# LANGUAGE OverloadedStrings #-}
module WordPatch where

import Data.Yaml
import Data.Set (Set)

data WordPatch = WordPatch { addWords  :: Set String
                           , dropWords :: Set String
                           } deriving (Show)

instance FromJSON WordPatch where
  parseJSON = withObject "WordPatch" $ \v -> WordPatch
    <$> v .: "add"
    <*> v .: "drop"

readWordPatch :: FilePath -> IO WordPatch
readWordPatch = decodeFileThrow
