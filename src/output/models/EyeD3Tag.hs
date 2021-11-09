module Output.Models.EyeD3Tag (EyeD3Tag(..), showCmd) where

import Prelude (Int, Show, String, show, (.), ($))
import Data.List (map)
import Data.Text (Text, intercalate, pack, unpack)
import System.Posix.Escape.Unicode (escape)
import Helpers ((◇), wrap)

data EyeD3Tag = ArtistParameter       Text
              | AlbumArtistParameter  Text
              | AlbumTitleParameter   Text
              | DiscNumParameter      Int
              | GenreParameter        Text
              | TrackNumParameter     Int
              | TrackTitleParameter   Text
              | YearParameter         Int

display ∷ Text → String
display = escape . unpack

instance Show EyeD3Tag where
  show (ArtistParameter      α) = "-a " ◇ (display α)
  show (AlbumArtistParameter α) = "-b " ◇ (display α)
  show (AlbumTitleParameter  α) = "-A " ◇ (display α)
  show (DiscNumParameter     α) = "-d " ◇ (show    α)
  show (GenreParameter       α) = "-G " ◇ (display α)
  show (TrackNumParameter    α) = "-n " ◇ (show    α)
  show (TrackTitleParameter  α) = "-t " ◇ (display α)
  show (YearParameter        α) = "-Y " ◇ (show    α)

-- Annoying. Consider something like BasicPrelude with Text Show
showCmd ∷ [EyeD3Tag] → Text
showCmd = intercalate " " . map (pack . show)
