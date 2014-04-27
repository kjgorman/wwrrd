{-#LANGUAGE OverloadedStrings #-}
module Track (
    Track
  , parseFile
  , lyricLength
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B

data Track = Track { media :: Maybe String, lyrics :: [String] }
             deriving (Show)

instance FromJSON Track where
  parseJSON (Object v) = Track <$>
                         v .:? "media" <*>
                         v .: "lyrics"
  parseJSON _ = mzero

parseFile :: FilePath -> IO (Maybe Track)
parseFile f = liftM decode $ B.readFile f

lyricLength :: Maybe Track -> Maybe Int
lyricLength m = do
  t <- m
  return . length $ lyrics t
