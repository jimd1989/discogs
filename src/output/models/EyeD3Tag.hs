module Output.Models.EyeD3Tag where

import Helpers ((◇), wrap)

-- Should all be strings?
data EyeD3Tag = ArtistParameter       String
              | AlbumArtistParameter  String
              | AlbumTitleParameter   String
              | DiscNumParameter      Int
              | GenreParameter        String
              | TrackNumParameter     Int
              | TrackTitleParameter   String
              | YearParameter         Int

instance Show EyeD3Tag where
  show (ArtistParameter      α) = "-a " ◇ (wrap α)
  show (AlbumArtistParameter α) = "-A " ◇ (wrap α)
  show (DiscNumParameter     α) = "-d " ◇ (show α)
  show (GenreParameter       α) = "-G " ◇ (wrap α)
  show (TrackNumParameter    α) = "-n " ◇ (show α)
  show (TrackTitleParameter  α) = "-t " ◇ (wrap α)
  show (YearParameter        α) = "-Y " ◇ (show α)
