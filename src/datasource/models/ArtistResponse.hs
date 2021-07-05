module Datasource.Models.ArtistResponse (ArtistResponse(..), ArtistResponse'(..)) where

import Prelude (String, Show)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ArtistResponse = ArtistResponse {
  name ∷ String,
  join ∷ String
} deriving (Generic, FromJSON, Show)

data ArtistResponse' = ArtistResponse' {
  name' ∷ Text,
  join' ∷ Text
} deriving (Generic, FromJSON, Show)
