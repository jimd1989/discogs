module Output.Transformers.AlbumResponseTransformer (transformAlbum) where

import Prelude (Bool, Either, String, (.), ($), (/=), (=<<), const, zipWith)
import Control.Error.Util (note)
import Control.Monad (join)
import Control.Monad.Except (MonadError)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (fromMaybe)
import Data.List (filter)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Datasource.Models.Flags (Flags, expand)
import Datasource.Models.AlbumResponse (AlbumResponse(..))
import Datasource.Models.TrackResponse (type_)
import Helpers ((⊙), (◇), fork, note')
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

removeHeadings ∷ AlbumResponse → AlbumResponse
removeHeadings α = α { tracklist = rm (tracklist α) }
  where rm = filter ((/= "heading") . type_) 

transformTrackList ∷ Bool → [EyeD3Tag] → EyeD3Tag → AlbumResponse → [[EyeD3Tag]]
transformTrackList exp constants artist = join . transform  . tracklist
  where transform   = parMap rpar (transformTracks exp constants artist)

transformAlbum ∷ MonadError String m ⇒ 
                 Flags → Text → AlbumResponse → m (NonEmpty Text)
transformAlbum flags specifiedGenre α = makeCmdList cmds
  where β           = removeHeadings α 
        year        = transformYear β
        genre       = GenreParameter specifiedGenre
        title       = transformTitle β
        albumArtist = transformAlbumArtist $ artists β
        constants   = [year, genre, title, albumArtist]
        artist      = transformArtist $ artists β
        tracks      = transformTrackList (expand flags) constants artist β
        positions   = transformPositions flags β
        cmds        = showCmd ⊙ zipWith (◇) tracks positions
        makeCmdList = note' "no EyeD3 commands generated" . nonEmpty
