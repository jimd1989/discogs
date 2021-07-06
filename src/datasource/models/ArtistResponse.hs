module Datasource.Models.ArtistResponse (ArtistResponse(..)) where

import Prelude (String, Show)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ArtistResponse = ArtistResponse {
  name ∷ Text,
  join ∷ Text
} deriving (Generic, FromJSON, Show)
