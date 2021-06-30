module Output.Models.EyeD3Parameter where

import Helpers ((◇), wrap)

data Flag = ArtistFlag
          | AlbumArtistFlag 
          | AlbumTitleFlag
          | DiscNumFlag
          | GenreFlag 
          | TrackNumFlag
          | TrackTitleFlag
          | YearFlag 

instance Show Flag where
  show ArtistFlag      = "-a "
  show AlbumArtistFlag = "-A "
  show DiscNumFlag     = "-d "
  show GenreFlag       = "-G "
  show TrackNumFlag    = "-n "
  show TrackTitleFlag  = "-t "
  show YearFlag        = "-Y "

data EyeD3Parameter = EyeD3Parameter {
  flag ∷ Flag,
  value ∷ String
}

instance Show EyeD3Parameter where
  show EyeD3Parameter{ flag = DiscNumFlag, value } = (show DiscNumFlag) ◇ value
  show EyeD3Parameter{ flag = TrackNumFlag, value} = (show TrackNumFlag) ◇ value
  show EyeD3Parameter{ flag = YearFlag, value }    = (show YearFlag) ◇ value
  show EyeD3Parameter{ flag, value }               = (show flag) ◇ (wrap value)
