module Datasource.Models.TrackResponse where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse)

data TrackResponse = TrackResponse {
  artists ∷ Maybe [ArtistResponse],
  position ∷ Text,
  sub_tracks ∷ Maybe [TrackResponse],
  title ∷ Text
} deriving (Generic, FromJSON, Show)
