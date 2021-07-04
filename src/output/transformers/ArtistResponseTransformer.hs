module Output.Transformers.ArtistResponseTransformer where

import Prelude (Char, String, (.), ($), (==), and, flip, pure)
import Control.Monad (guard)
import Data.Functor ((<$))
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, fromList, member)
import Data.Traversable (traverse)
import Datasource.Models.ArtistResponse (ArtistResponse(..), join, name)
import Helpers ((◁), (◀), (⊙), (◇), fork, head', last', validate)
import Output.Models.EyeD3Tag (EyeD3Tag(..))
import Output.Transformers.TextTransformer (checkCaps, fromWords, 
                                            onLast, onTail, onWords)

tag ∷ Set Char
tag = fromList "(0123456789)"

filterTag ∷ String → [String]
filterTag = fork fromMaybe pure deleteTag
  where match α   = guard . (== α)
        isInTag   = guard . (and . (flip member tag ⊙))
        criteria  = [isInTag, match '(' ◀ head', match ')' ◀ last']
        deleteTag = ([] <$) . validate criteria

checkJoin ∷ String → String
checkJoin ""  = ""
checkJoin "," = ", "
checkJoin "/" = "/"
checkJoin α   = " " ◇ α ◇ " "

transformName ∷ ArtistResponse → Maybe String
transformName = fromWords ◁ onWords (onTail checkCaps ◁ onLast filterTag) . name

transformArtistResponse ∷ ArtistResponse → Maybe String
transformArtistResponse α =  (◇ (checkJoin $ join α)) ⊙ (transformName α)

transformArtists ∷ [ArtistResponse] → Maybe String
transformArtists = intercalate "" ◁ traverse transformArtistResponse

transformAlbumArtist ∷ [ArtistResponse] → EyeD3Tag
transformAlbumArtist α = case transformArtists α of
  (Just "Various") → AlbumArtistParameter "Various Artists"
  (Just ω        ) → AlbumArtistParameter ω
  (Nothing       ) → AlbumArtistParameter ""

emptyArtist ∷ EyeD3Tag
emptyArtist = ArtistParameter ""

transformArtist ∷ [ArtistResponse] → EyeD3Tag
transformArtist = fromMaybe emptyArtist . (ArtistParameter ◁ transformArtists)
