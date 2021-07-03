module Output.Transformers.AlbumResponseTransformer where

import Data.Maybe (fromMaybe)
import Datasource.Models.AlbumResponse (AlbumResponse, artists, title,
                                        tracklist, year)
import Helpers ((⊙), (◇), fork)
import Output.Models.EyeD3Tag (EyeD3Tag(..), showCmd)
import Output.Transformers.ArtistResponseTransformer (transformArtist, 
                                                      transformAlbumArtist)
import Output.Transformers.PositionsTransformer (transformPositions)
import Output.Transformers.TextTransformer (transformText)
import Output.Transformers.TrackResponseTransformer (transformTracks)

transformYear ∷ AlbumResponse → EyeD3Tag
transformYear = YearParameter . fork fromMaybe (const 0) year

transformTitle ∷ AlbumResponse → EyeD3Tag
transformTitle = AlbumTitleParameter . transformText . title 

transformAlbum ∷ Bool → String → AlbumResponse → [String]
transformAlbum expand specifiedGenre α = showCmd ⊙ zipWith (◇) tracks positions
  where year        = transformYear α
        genre       = GenreParameter specifiedGenre
        title       = transformTitle α
        albumArtist = transformAlbumArtist $ artists α
        constants   = [Command, year, genre, title, albumArtist]
        artist      = transformArtist $ artists α
        tracks      = transformTracks expand constants artist =<< tracklist α
        positions   = transformPositions expand α
