module Datasource.Models.TrackResponse (TrackResponse(..)) where

import Prelude (Maybe, Show, String)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse)

data TrackResponse = TrackResponse {
  artists ∷ Maybe [ArtistResponse],
  position ∷ String,
  sub_tracks ∷ Maybe [TrackResponse],
  title ∷ String
} deriving (Generic, FromJSON, Show)
