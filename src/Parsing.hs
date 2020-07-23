module Parsing where

import Control.Applicative ((<|>))
import Control.Monad (liftM2, mapM)
import Data.Aeson (FromJSON, (.:), (.:?), (.!=), decode, parseJSON, 
                   withArray, withObject)
import Data.Aeson.Types (Parser, Value(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as U
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe, isJust)
import Data.Text (Text)
import Data.Tuple (uncurry)
import Data.Vector (toList)
import FormatTitle (formatArtist, formatTitle)
import FormatTrack (Position, position)
import Helpers (dyfork, enumerate, maybeIf)

type Track = (Text, Position, Text)

parseName ∷ Value → Parser Text
parseName = withObject "artist" $ dyfork formatArtist (.: "name") (.: "join")

parseArtist ∷ Value → Parser Text
parseArtist = withArray "[a]" $ fmap (foldl1 (<>)) . mapM parseName . toList

isTrack ∷ Value → Bool
isTrack (Object α) = isJust $ HM.lookup "type_" α >>= maybeIf acceptable
                       where acceptable α = (α == "track") || (α == "index")
isTrack  _         = False

parseTrack ∷ Text → Int → Value → Parser Track
parseTrack ω n = withObject "track" $ \α → do
  title    ← α .: "title"
  position ← (position n) <$> (α .: "position")
  artist   ← (parseArtist =<< (α .: "artists")) <|> (pure ω)
  return (artist, position, title)

parseTracks ∷ Text → Value → Parser [Track]
parseTracks ω = withArray "[a]" $ mapM (uncurry (parseTrack ω)) . tracks
  where tracks = enumerate . filter isTrack . toList

data Album = Album {year ∷ Int, artist ∷ Text, album ∷ Text, tracks ∷ [Track]}
  deriving (Show)

instance FromJSON Album where
  parseJSON = withObject "album" $ \α → do
    year   ← α .:? "year" .!= 0
    artist ← parseArtist =<< (α .: "artists")
    album  ← formatTitle <$> (α .: "title")
    tracks ← parseTracks artist =<< (α .: "tracklist")
    return Album{..}

decode' ∷ [Char] → Maybe Album
decode' = decode . BS.fromStrict . U.fromString
