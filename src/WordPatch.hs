{-# LANGUAGE OverloadedStrings #-}
module WordPatch where

import Data.Yaml
import Data.Set (Set, empty)
import Data.Text (Text)

data WordPatch = WordPatch { addWords  :: Set Text
                           , dropWords :: Set Text
                           } deriving (Show)

instance FromJSON WordPatch where
  parseJSON = withObject "WordPatch" $ \v -> WordPatch
    <$> v .:? "add" .!= empty
    <*> v .:? "drop" .!= empty

readWordPatch :: FilePath -> IO WordPatch
readWordPatch = decodeFileThrow
