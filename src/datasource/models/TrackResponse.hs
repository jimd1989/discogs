module Datasource.Models.TrackResponse (TrackResponse(..)) where

import Prelude (Maybe, Show, String)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse)

data TrackResponse = TrackResponse {
  artists ∷ Maybe [ArtistResponse],
  position ∷ Text,
  sub_tracks ∷ Maybe [TrackResponse],
  title ∷ Text,
  type_ ∷ Text
} deriving (Generic, FromJSON, Show)
