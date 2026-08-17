module Datasource.Models.AlbumResponse (AlbumResponse(..)) where

import Prelude (Int, Maybe, Show)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse)
import Datasource.Models.TrackResponse (TrackResponse)

data AlbumResponse = AlbumResponse {
  artists ∷ [ArtistResponse],
  title ∷ Text,
  tracklist ∷ [TrackResponse],
  year ∷ Maybe Int
} deriving (Generic, FromJSON, Show)

