module Datasource.Models.AlbumResponse where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse)
import Datasource.Models.TrackResponse (TrackResponse)

data AlbumResponse = AlbumResponse {
  artists ∷ [ArtistResponse],
  title ∷ String,
  tracklist ∷ [TrackResponse],
  year ∷ Maybe Int
} deriving (Generic, FromJSON, Show)
