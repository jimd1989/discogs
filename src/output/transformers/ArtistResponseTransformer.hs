module Output.Transformers.ArtistResponseTransformer where

import Control.Monad (guard)
import Data.Functor ((<$))
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, fromList, member)
import Data.Traversable (traverse)
import Datasource.Models.ArtistResponse (ArtistResponse(..), join, name)
import Helpers ((◁), (◀), (⊙), (◇), fork, head', last', validate)
import Output.Models.EyeD3Parameter (EyeD3Parameter(..))
import Output.Transformers.TextTransformer (checkCaps, onLast, onTail, onWords)

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
transformName = onWords (onTail checkCaps ◁ onLast filterTag) . name

transformArtistResponse ∷ ArtistResponse → Maybe String
transformArtistResponse α =  (◇ (checkJoin $ join α)) ⊙ (transformName α)

transformArtists ∷ [ArtistResponse] → Maybe String
transformArtists = intercalate "" ◁ traverse transformArtistResponse

transformAlbumArtist ∷ [ArtistResponse] → EyeD3Parameter
transformAlbumArtist α = case transformArtists α of
  (Just "Various") → AlbumArtistParameter "Various Artists"
  (Just ω        ) → AlbumArtistParameter ω
  (Nothing       ) → AlbumArtistParameter ""

transformArtist ∷ EyeD3Parameter → [ArtistResponse] → EyeD3Parameter
transformArtist α = fromMaybe α . (ArtistParameter ◁ transformArtists)

emptyArtist ∷ EyeD3Parameter
emptyArtist = ArtistParameter ""
