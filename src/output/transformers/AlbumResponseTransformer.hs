module Output.Transformers.AlbumResponseTransformer (transformAlbum) where

import Prelude (Either, String, (.), ($), (=<<), const, zipWith)
import Control.Error.Util (note)
import Control.Monad (join)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Text as T
import Datasource.Models.Flags (Flags, expand)
import Datasource.Models.AlbumResponse (AlbumResponse(..), AlbumResponse'(..))
import Helpers ((⊙), (◇), fork)
import Output.Models.EyeD3Tag (EyeD3Tag(..), EyeD3Tag'(..), showCmd, showCmd')
import Output.Transformers.ArtistResponseTransformer (transformArtist, 
                                                      transformAlbumArtist, transformArtist', transformAlbumArtist')
import Output.Transformers.PositionsTransformer (transformPositions, transformPositions')
import Output.Transformers.TextTransformer (transformText, transformText')
import Output.Transformers.TrackResponseTransformer (transformTracks, transformTracks')

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
        constants   = [year, genre, title, albumArtist]
        artist      = transformArtist $ artists α
        exp         = expand flags 
        trackMap    = join . parMap rpar (transformTracks exp constants artist)
        tracks      = trackMap $ tracklist α
        positions   = transformPositions flags α
        cmds        = showCmd ⊙ zipWith (◇) tracks positions
        makeCmdList = note "no EyeD3 commands generated" . nonEmpty

--new
transformYear' ∷ AlbumResponse' → EyeD3Tag'
transformYear' = YearParameter' . fork fromMaybe (const 0) year'

transformTitle' ∷ AlbumResponse' → EyeD3Tag'
transformTitle' = AlbumTitleParameter' . transformText' . title'

transformAlbum' ∷ Flags → T.Text → AlbumResponse' → Either T.Text (NonEmpty T.Text)
transformAlbum' flags specifiedGenre α = makeCmdList cmds
  where year        = transformYear' α
        genre       = GenreParameter' specifiedGenre
        title       = transformTitle' α
        albumArtist = transformAlbumArtist' $ artists' α
        constants   = [year, genre, title, albumArtist]
        artist      = transformArtist' $ artists' α
        exp         = expand flags 
        trackMap    = join . parMap rpar (transformTracks' exp constants artist)
        tracks      = trackMap $ tracklist' α
        positions   = transformPositions' flags α
        cmds        = showCmd' ⊙ zipWith (◇) tracks positions
        makeCmdList = note (T.pack $ "no EyeD3 commands generated") . nonEmpty
