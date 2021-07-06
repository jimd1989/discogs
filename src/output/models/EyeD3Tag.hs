module Output.Models.EyeD3Tag (EyeD3Tag(..), showCmd, EyeD3Tag'(..), showCmd') where

import Prelude (Int, Show, String, show, (.), ($))
import Data.List (map, intercalate)
import qualified Data.Text as T
import Helpers ((◇), wrap)

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
  show (AlbumArtistParameter α) = "-b " ◇ (wrap α)
  show (AlbumTitleParameter α)  = "-A " ◇ (wrap α)
  show (DiscNumParameter     α) = "-d " ◇ (show α)
  show (GenreParameter       α) = "-G " ◇ (wrap α)
  show (TrackNumParameter    α) = "-n " ◇ (show α)
  show (TrackTitleParameter  α) = "-t " ◇ (wrap α)
  show (YearParameter        α) = "-Y " ◇ (show α)

showCmd ∷ [EyeD3Tag] → String
showCmd = intercalate " " . map show

--new
data EyeD3Tag' = ArtistParameter'       T.Text
               | AlbumArtistParameter'  T.Text
               | AlbumTitleParameter'   T.Text
               | DiscNumParameter'      Int
               | GenreParameter'        T.Text
               | TrackNumParameter'     Int
               | TrackTitleParameter'   T.Text
               | YearParameter'         Int

instance Show EyeD3Tag' where
  show (ArtistParameter'      α) = "-a " ◇ (wrap $ show α)
  show (AlbumArtistParameter' α) = "-b " ◇ (wrap $ show α)
  show (AlbumTitleParameter'  α) = "-A " ◇ (wrap $ show α)
  show (DiscNumParameter'     α) = "-d " ◇ (show α)
  show (GenreParameter'       α) = "-G " ◇ (wrap $ show α)
  show (TrackNumParameter'    α) = "-n " ◇ (show α)
  show (TrackTitleParameter'  α) = "-t " ◇ (wrap $ show α)
  show (YearParameter'        α) = "-Y " ◇ (show α)

-- Annoying. Consider something like BasicPrelude with Text Show
showCmd' ∷ [EyeD3Tag'] → T.Text
showCmd' = T.intercalate " " . map (T.pack . show)
