module Output.Transformers.TrackResponseTransformer where

import Prelude (Bool(..), Maybe(..), (.), ($), pure)
import Data.Maybe (fromMaybe)
import Helpers ((◇), (⊙))
import Datasource.Models.TrackResponse (TrackResponse, artists, 
                                        sub_tracks, title)
import Output.Models.EyeD3Tag (EyeD3Tag(..))
import Output.Transformers.ArtistResponseTransformer (transformArtist)
import Output.Transformers.TextTransformer (transformText)

transformTitle ∷ TrackResponse → [EyeD3Tag]
transformTitle = pure . TrackTitleParameter . transformText . title

transformTrack ∷ [EyeD3Tag] → EyeD3Tag → TrackResponse → [EyeD3Tag]
transformTrack constants artist α = constants ◇ trackArtist ◇ transformTitle α
  where trackArtist = pure $ fromMaybe artist (transformArtist ⊙ artists α)

-- 2D matrix of all (sub)track tags, sans positions
transformTracks ∷ Bool → [EyeD3Tag] → EyeD3Tag → TrackResponse → [[EyeD3Tag]]
transformTracks expand constants artist α = case (expand, sub_tracks α) of
  (True, (Just ω)) → (transformTrack constants artist) ⊙ ω
  (_,      _     ) → pure $ transformTrack constants artist α
