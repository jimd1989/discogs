module Output.Transformers.AlbumResponseTransformer (transformAlbum) where

import Prelude (Either, String, (.), ($), (=<<), const, zipWith)
import Control.Error.Util (note)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Datasource.Models.Flags (Flags, expand)
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

transformAlbum ∷ Flags → String → AlbumResponse → Either String(NonEmpty String)
transformAlbum flags specifiedGenre α = makeCmdList cmds
  where year        = transformYear α
        genre       = GenreParameter specifiedGenre
        title       = transformTitle α
        albumArtist = transformAlbumArtist $ artists α
        constants   = [Command, year, genre, title, albumArtist]
        artist      = transformArtist $ artists α
        exp         = expand flags 
        tracks      = transformTracks exp constants artist =<< tracklist α
        positions   = transformPositions flags α
        cmds        = showCmd ⊙ zipWith (◇) tracks positions
        makeCmdList = note "no EyeD3 commands generated" . nonEmpty
