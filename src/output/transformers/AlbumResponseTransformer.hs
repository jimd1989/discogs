module Output.Transformers.AlbumResponseTransformer (transformAlbum) where

import Prelude (Bool, Either, String, (.), ($), (=<<), const, zipWith)
import Control.Error.Util (note)
import Control.Monad (join)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Datasource.Models.Flags (Flags, expand)
import Datasource.Models.AlbumResponse (AlbumResponse(..))
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

transformTrackList ∷ Bool → [EyeD3Tag] → EyeD3Tag → AlbumResponse → [[EyeD3Tag]]
transformTrackList exp constants artist = join . transform  . tracklist
  where transform = parMap rpar (transformTracks exp constants artist)

transformAlbum ∷ Flags → Text → AlbumResponse → Either String (NonEmpty Text)
transformAlbum flags specifiedGenre α = makeCmdList cmds
  where year        = transformYear α
        genre       = GenreParameter specifiedGenre
        title       = transformTitle α
        albumArtist = transformAlbumArtist $ artists α
        constants   = [year, genre, title, albumArtist]
        artist      = transformArtist $ artists α
        tracks      = transformTrackList (expand flags) constants artist α
        positions   = transformPositions flags α
        cmds        = showCmd ⊙ zipWith (◇) tracks positions
        makeCmdList = note "no EyeD3 commands generated" . nonEmpty
