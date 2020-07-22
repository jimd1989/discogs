module Parse where

import Control.Applicative ((<|>))
import Control.Monad (guard, liftM2, mapM)
import Data.Aeson (FromJSON, (.:), (.:?), (.!=), eitherDecode, parseJSON, 
                   withArray, withObject)
import Data.Aeson.Types (Parser, Value)
import qualified Data.ByteString.Lazy as BS
import Data.Functor (($>))
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text)
import Data.Vector (toList)
import GHC.Generics (Generic)
import FormatTitle (formatArtist, formatTitle)
import Helpers (dyfork, maybeIf)

type Track = (Text, Text, Text)

parseName ∷ Value → Parser Text
parseName = withObject "artist" $ dyfork formatArtist (.: "name") (.: "join")

parseArtist ∷ Value → Parser Text
parseArtist = withArray "[a]" $ fmap (foldl1 (<>)) . mapM parseName . toList

parseTrack ∷ Text → Value → Parser (Maybe Track)
parseTrack ω = withObject "track" $ \α → do
  type_    ← maybeIf (== ("track" ∷ Text)) <$> (α .: "type_")
  title    ← α .: "title"
  position ← α .: "position"
  artist   ← (parseArtist =<< (α .: "artists")) <|> (pure ω)
  return (type_ $> (artist, position, title))

parseTracks ∷ Text → Value → Parser [Track]
parseTracks ω = withArray "[a]" $ fmap catMaybes . mapM (parseTrack ω) . toList

data Release = Release { year ∷ Int,
                         artist ∷ Text,
                         album ∷ Text,
                         tracks ∷ [Track] } deriving (Generic, Show)

instance FromJSON Release where
  parseJSON = withObject "release" $ \α → do
    year   ← α .:? "year" .!= 0
    artist ← parseArtist =<< (α .: "artists")
    album  ← formatTitle <$> (α .: "title")
    tracks ← parseTracks artist =<< (α .: "tracklist")
    return Release{..}

getJSON ∷ [Char] → IO BS.ByteString
getJSON = BS.readFile

-- (eitherDecode <$> getJSON "file") :: IO (Either String Release)
