{-# LANGUAGE OverloadedStrings #-}
module PhraseSet where

import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson
import qualified Data.Set as S
import           NLP.WordNet
import           Track

data PhraseSet = PhraseSet { track :: Track
                           , phraseLines :: [PhraseLine] }
                 deriving (Show)

instance FromJSON PhraseSet where
  parseJSON (Object v) = PhraseSet <$>
                         v .: "track" <*>
                         v .: "phraseLines"
  parseJSON _ = mzero

instance ToJSON PhraseSet where
  toJSON phrase = object [ "track"   .= toJSON (track phrase)
                         , "phraseLines" .= toJSON (phraseLines phrase)]

data PhraseLine = PhraseLine { line :: String, phrases :: S.Set Word }
                  deriving (Show)

instance FromJSON PhraseLine where
  parseJSON (Object v) = PhraseLine <$>
                          v .: "line" <*>
                          v .: "phrases"
  parseJSON _ = mzero

instance ToJSON PhraseLine where
  toJSON phraseLine = object [ "line" .= line phraseLine
                             , "phrases" .= phrases phraseLine]
