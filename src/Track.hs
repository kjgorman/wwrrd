{-#LANGUAGE OverloadedStrings #-}
module Track (
    Track
  , title
  , media
  , lyrics
  , parseDirectory
  ) where

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad (liftM, mzero)
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           System.Directory

data Track = Track { title :: String
                   , media :: Maybe String
                   , lyrics :: [String] }
             deriving (Show)

instance FromJSON Track where
  parseJSON (Object v) = Track <$>
                         v .: "title" <*>
                         v .:? "media" <*>
                         v .: "lyrics"
  parseJSON _ = mzero

instance ToJSON Track where
  toJSON track = object ["media" .= (media track), "lyrics" .= (lyrics track)]

parseFile :: FilePath -> IO (Maybe Track)
parseFile f = liftM (decode . BL.fromStrict) $ B.readFile f

parseDirectory :: FilePath -> IO [Track]
parseDirectory dir = do
  f <- getDirectoryContents dir
  files <- mapM (\f -> parseFile $ dir ++ "/" ++ f) (drop 2 f)
  return $ map fromJust $ filter isJust files
