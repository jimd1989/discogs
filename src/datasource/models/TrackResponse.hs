module Datasource.Models.TrackResponse (TrackResponse(..), TrackResponse'(..)) where

import Prelude (Maybe, Show, String)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse, ArtistResponse')

data TrackResponse = TrackResponse {
  artists ∷ Maybe [ArtistResponse],
  position ∷ String,
  sub_tracks ∷ Maybe [TrackResponse],
  title ∷ String
} deriving (Generic, FromJSON, Show)

data TrackResponse' = TrackResponse' {
  artists' ∷ Maybe [ArtistResponse'],
  position' ∷ T.Text,
  sub_tracks' ∷ Maybe [TrackResponse'],
  title' ∷ T.Text
} deriving (Generic, FromJSON, Show)
