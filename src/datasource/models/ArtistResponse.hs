module Datasource.Models.ArtistResponse where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data ArtistResponse = ArtistResponse {
  name ∷ String,
  join ∷ String
} deriving (Generic, FromJSON, Show)
