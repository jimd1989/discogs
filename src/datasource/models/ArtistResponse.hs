module Datasource.Models.ArtistResponse where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ArtistResponse = ArtistResponse {
  name ∷ Text,
  join ∷ Text
} deriving (Generic, FromJSON, Show)
