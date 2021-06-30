module Output.Transformers.TrackTitleTransformer where

import Datasource.Models.TrackResponse (TrackResponse, title)
import Output.Models.EyeD3Parameter (EyeD3Parameter(..))
import Output.Transformers.TextTransformer (transformText)

transformTrackTitle ∷ TrackResponse → EyeD3Parameter
transformTrackTitle = transformText TrackTitleParameter . title
