module Parsing where

import Control.Applicative ((<|>))
import Control.Error.Util (note)
import Control.Monad (mapM)
import Data.Aeson ((.:), (.:?), (.!=), eitherDecode, json, withArray, withObject)
import Data.Aeson.Parser (decodeWith)
import Data.Aeson.Types (Parser, Value(..), parse)
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple (uncurry)
import Data.Vector (toList)
import Datasource.Models.AlbumResponse (AlbumResponse)
import FormatTitle (formatArtist, formatTitle)
import FormatTrack (Position, Position', makePosition, position)
import Helpers ((⊙), (◁), (◇), dyfork, enumerate')

-- Hopefully lenses will simplify things here

type Track = (Text, Position, Text)

parseName ∷ Value → Parser Text
parseName = withObject "artist" $ dyfork formatArtist (.: "name") (.: "join")

parseArtist ∷ Value → Parser Text
parseArtist = withArray "[a]" $ foldl1 (◇) ◁ mapM parseName . toList

filterTracks ∷ Bool → [Value] → [Value]
filterTracks _ []             = []
filterTracks expand ((Object α):ω) = 
  let ls                   = fromMaybe [Null] . ls' . HM.lookup "sub_tracks"
      ls' (Just (Array α)) = Just $ toList α
      ls' _                = Nothing
  in case (expand, HM.lookup "type_" α) of
    (_,     Just "track") → (Object α) : (filterTracks expand ω)
    (True,  Just "index") → (ls α)     ◇ (filterTracks expand ω)
    (False, Just "index") → (Object α) : (filterTracks expand ω)
    (_,     _)            →               filterTracks expand ω
filterTracks expand (α:ω)          = α : (filterTracks expand ω)

parseTrack ∷ Text → Int → Value → Parser Track
parseTrack aArtist fallbackNum = withObject "track" $ \α → do
  title    ← formatTitle  ⊙ (α .: "title")
  position ← (FormatTrack.position fallbackNum) ⊙ (α .: "position")
  artist   ← (parseArtist =<< (α .: "artists")) <|> (pure aArtist)
  return (artist, position, title)

parseTracks ∷ Bool → Text → Value → Parser [Track]
parseTracks expand ω = withArray "[a]" $ mapM (uncurry (parseTrack ω)) . tracks
  where tracks = enumerate' . filterTracks expand . toList

data Album = Album {year ∷ Int, artist ∷ Text, album ∷ Text, tracks ∷ [Track]}
  deriving (Show)

decode' ∷ Bool → ByteString → Either String Album
decode' expand = note "error decoding JSON" . decoder
  where decoder = decodeWith json (parse parser)
        parser  = withObject "album" $ \α → do
           year   ← α .:? "year" .!= 0
           artist ← parseArtist =<< (α .: "artists")
           album  ← formatTitle ⊙ (α .: "title")
           tracks ← parseTracks expand artist =<< (α .: "tracklist")
           return Album{..}

-- new
-- maybe Position' should stay as Text?
--data Track' = Track' { title ∷ Text, position ∷ Position', artist' ∷ Text }

--parseTrack' ∷ Text → Int → Value → Parser Track'
--parseTrack' aArtist fallbackNum = withObject "track" $ \α → do
--  title    ← formatTitle  ⊙ (α .: "title")
  --position ← (makePosition fallbackNum) =<< (α .: "position") -- Aeson error vs Either: needs its own parser
--  artist   ← (parseArtist =<< (α .: "artists")) <|> (pure aArtist)
--  pure $ Track' title position artist

decode'' ∷ ByteString → Either String AlbumResponse
decode'' α = eitherDecode α
