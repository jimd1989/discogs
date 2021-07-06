module Datasource.Models.AlbumResponse (AlbumResponse(..), AlbumResponse'(..)) where

import Prelude (Int, Maybe, Show, String)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Datasource.Models.ArtistResponse (ArtistResponse, ArtistResponse')
import Datasource.Models.TrackResponse (TrackResponse, TrackResponse')

data AlbumResponse = AlbumResponse {
  artists ∷ [ArtistResponse],
  title ∷ String,
  tracklist ∷ [TrackResponse],
  year ∷ Maybe Int
} deriving (Generic, FromJSON, Show)

data AlbumResponse' = AlbumResponse' {
  artists' ∷ [ArtistResponse'],
  title' ∷ T.Text,
  tracklist' ∷ [TrackResponse'],
  year' ∷ Maybe Int
} deriving (Generic, FromJSON, Show)

