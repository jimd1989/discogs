module Output.Models.EyeD3Tag where

import Prelude (Int, Show, String, show, (.))
import Data.List (map, intercalate)
import Helpers ((◇), wrap)

data EyeD3Tag = Command
              | ArtistParameter       String
              | AlbumArtistParameter  String
              | AlbumTitleParameter   String
              | DiscNumParameter      Int
              | GenreParameter        String
              | TrackNumParameter     Int
              | TrackTitleParameter   String
              | YearParameter         Int

instance Show EyeD3Tag where
  show  Command                 = "eyeD3 "
  show (ArtistParameter      α) = "-a " ◇ (wrap α)
  show (AlbumArtistParameter α) = "-b " ◇ (wrap α)
  show (AlbumTitleParameter α)  = "-A " ◇ (wrap α)
  show (DiscNumParameter     α) = "-d " ◇ (show α)
  show (GenreParameter       α) = "-G " ◇ (wrap α)
  show (TrackNumParameter    α) = "-n " ◇ (show α)
  show (TrackTitleParameter  α) = "-t " ◇ (wrap α)
  show (YearParameter        α) = "-Y " ◇ (show α)

showCmd ∷ [EyeD3Tag] → String
showCmd = intercalate " " . map show
