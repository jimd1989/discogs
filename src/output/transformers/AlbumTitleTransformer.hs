module Output.Transformers.AlbumTitleTransformer where

import Datasource.Models.AlbumResponse (AlbumResponse, title)
import Output.Models.EyeD3Parameter (EyeD3Parameter(..))
import Output.Transformers.TextTransformer (transformText)

transformAlbumTitle ∷ AlbumResponse → EyeD3Parameter
transformAlbumTitle = transformText AlbumTitleParameter . title
