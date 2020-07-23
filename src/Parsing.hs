module Parsing where

import Control.Applicative ((<|>))
import Control.Monad (mapM)
import Data.Aeson ((.:), (.:?), (.!=), json, withArray, withObject)
import Data.Aeson.Parser (decodeWith)
import Data.Aeson.Types (Parser, Value(..), parse)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as U
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe, fromMaybe)
import Data.Text (Text)
import Data.Tuple (uncurry)
import Data.Vector (toList)
import FormatTitle (formatArtist, formatTitle)
import FormatTrack (Position, position)
import Helpers (dyfork, enumerate)

type Track = (Text, Position, Text)

parseName ∷ Value → Parser Text
parseName = withObject "artist" $ dyfork formatArtist (.: "name") (.: "join")

parseArtist ∷ Value → Parser Text
parseArtist = withArray "[a]" $ fmap (foldl1 (<>)) . mapM parseName . toList

filterTracks ∷ Bool → [Value] → [Value]
filterTracks _ []             = []
filterTracks expand ((Object α):ω) = 
  let ls                   = fromMaybe [Null] . ls' . HM.lookup "sub_tracks"
      ls' (Just (Array α)) = Just $ toList α
      ls' _                = Nothing
  in case (expand, HM.lookup "type_" α) of
    (_,     Just "track") → (Object α) : (filterTracks expand ω)
    (True,  Just "index") → (ls α)    <> (filterTracks expand ω)
    (False, Just "index") → (Object α) : (filterTracks expand ω)
    (_,     _)            →               filterTracks expand ω
filterTracks expand (α:ω)          = α : (filterTracks expand ω)

parseTrack ∷ Text → Int → Value → Parser Track
parseTrack ω n = withObject "track" $ \α → do
  title    ← α .: "title"
  position ← (position n) <$> (α .: "position")
  artist   ← (parseArtist =<< (α .: "artists")) <|> (pure ω)
  return (artist, position, title)

parseTracks ∷ Bool → Text → Value → Parser [Track]
parseTracks expand ω = withArray "[a]" $ mapM (uncurry (parseTrack ω)) . tracks
  where tracks = enumerate . filterTracks expand . toList

data Album = Album {year ∷ Int, artist ∷ Text, album ∷ Text, tracks ∷ [Track]}
  deriving (Show)

decode' ∷ Bool → [Char] → Maybe Album
decode' expand = decodeWith json (parse parser) . BS.fromStrict . U.fromString
  where parser = withObject "album" $ \α → do
           year   ← α .:? "year" .!= 0
           artist ← parseArtist =<< (α .: "artists")
           album  ← formatTitle <$> (α .: "title")
           tracks ← parseTracks expand artist =<< (α .: "tracklist")
           return Album{..}
