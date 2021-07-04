module Datasource.Models.TrackResponse where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse)

data TrackResponse = TrackResponse {
  artists ∷ Maybe [ArtistResponse],
  position ∷ String,
  sub_tracks ∷ Maybe [TrackResponse],
  title ∷ String
} deriving (Generic, FromJSON, Show)
