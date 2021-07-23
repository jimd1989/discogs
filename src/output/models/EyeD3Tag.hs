module Output.Models.EyeD3Tag (EyeD3Tag(..), showCmd) where

import Prelude (Int, Show, String, show, (.), ($))
import Data.List (map)
import Data.Text (Text, intercalate, pack)
import Helpers ((◇), wrap)

data EyeD3Tag = ArtistParameter       Text
              | AlbumArtistParameter  Text
              | AlbumTitleParameter   Text
              | DiscNumParameter      Int
              | GenreParameter        Text
              | TrackNumParameter     Int
              | TrackTitleParameter   Text
              | YearParameter         Int

instance Show EyeD3Tag where
  show (ArtistParameter      α) = "-a " ◇ (show α)
  show (AlbumArtistParameter α) = "-b " ◇ (show α)
  show (AlbumTitleParameter  α) = "-A " ◇ (show α)
  show (DiscNumParameter     α) = "-d " ◇ (show α)
  show (GenreParameter       α) = "-G " ◇ (show α)
  show (TrackNumParameter    α) = "-n " ◇ (show α)
  show (TrackTitleParameter  α) = "-t " ◇ (show α)
  show (YearParameter        α) = "-Y " ◇ (show α)

-- Annoying. Consider something like BasicPrelude with Text Show
showCmd ∷ [EyeD3Tag] → Text
showCmd = intercalate " " . map (pack . show)
