module Parse where

import Control.Monad ((<=<), liftM2, mapM)
import Data.Aeson (FromJSON, (.:), (.:?), (.!=), eitherDecode, parseJSON, 
                   withArray, withObject)
import Data.Aeson.Types (Parser, Value)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Vector (toList)
import GHC.Generics (Generic)
import FormatTitle (formatArtist, formatTitle)

parseName ∷ Value → Parser Text
parseName = withObject "artist" $
            (liftM2 . liftM2) formatArtist (.: "name") (.: "join")

parseArtist ∷ Value → Parser Text
parseArtist = concatNames <=< getNames
  where concatNames = pure . foldl1 (<>)
        getNames    = withArray "[a]" $ mapM parseName . toList

data Artist = Artist { name ∷ Text,
                      join ∷ Text } deriving (Generic, Show)

instance FromJSON Artist

data Track = Track { title ∷ Text,
                     position ∷ Text,
                     artists ∷ Maybe [Artist] } deriving (Generic, Show)

instance FromJSON Track

data Release = Release { year ∷ Int,
                         albumArtist ∷ Text,
                         albumTitle ∷ Text,
                         tracks ∷ [Track] } deriving (Generic, Show)

instance FromJSON Release where
  parseJSON = withObject "release" $ \α → do
    year        ← α .:? "year" .!= 0
    albumArtist ← parseArtist =<< (α .:  "artists")
    albumTitle  ← formatTitle <$> (α .:  "title")
    tracks      ← α .:  "tracklist"
    return Release{..}

getJSON ∷ [Char] → IO BS.ByteString
getJSON = BS.readFile

-- (eitherDecode <$> getJSON "file") :: IO (Either String Release)
