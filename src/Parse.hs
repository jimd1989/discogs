module Parse where

import Control.Applicative ((<|>))
import Control.Monad (liftM2, mapM)
import Data.Aeson (FromJSON, (.:), (.:?), (.!=), eitherDecode, parseJSON, 
                   withArray, withObject)
import Data.Aeson.Types (Parser, Value(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Vector (toList)
import GHC.Generics (Generic)
import FormatTitle (formatArtist, formatTitle)
import FormatTrack (Position, position)
import Helpers (dyfork, maybeIf)

type Track = (Text, Position, Text)

parseName ∷ Value → Parser Text
parseName = withObject "artist" $ dyfork formatArtist (.: "name") (.: "join")

parseArtist ∷ Value → Parser Text
parseArtist = withArray "[a]" $ fmap (foldl1 (<>)) . mapM parseName . toList

isTrack ∷ Value → Bool
isTrack (Object α) = isJust $ HM.lookup "type_" α >>= maybeIf (== "track")
isTrack  _         = False

parseTrack ∷ Text → Value → Parser Track
parseTrack ω = withObject "track" $ \α → do
  title    ← α .: "title"
  position ← (position 1) <$> (α .: "position")
  artist   ← (parseArtist =<< (α .: "artists")) <|> (pure ω)
  return (artist, position, title)

parseTracks ∷ Text → Value → Parser [Track]
parseTracks ω = withArray "[a]" $ mapM (parseTrack ω) . filter isTrack . toList

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
