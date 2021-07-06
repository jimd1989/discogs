module Output.Transformers.ArtistResponseTransformer
  (transformAlbumArtist, transformArtist)
where

import Prelude (Bool(..), Char, String, (.), ($), (==), (&&), and, flip, pure)
import Control.Monad (guard)
import Data.Functor ((<$))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, fromList, member)
import Data.Text (Text, foldr, intercalate, unpack)
import Data.Traversable (traverse)
import Datasource.Models.ArtistResponse (ArtistResponse(..))
import Helpers ((◁), (◀), (⊙), (◇), fork, head', last', validate)
import Output.Models.EyeD3Tag (EyeD3Tag(..))
import Output.Transformers.TextTransformer (checkCaps, fromWords, onLast, 
                                            onTail, onWords, smushWords)

tag ∷ Set Char
tag = fromList "(0123456789)"

filterTag ∷ Text → [Text]
filterTag = fork fromMaybe pure deleteTag
  where match α f = guard . (== α) ◀ f . unpack
        isInTag   = guard . foldr ((&&) . (flip member tag)) True
        criteria  = [isInTag, match '(' head', match ')' last']
        deleteTag = ([] <$) . validate criteria

checkJoin ∷ Text → Text
checkJoin ""  = ""
checkJoin "," = ", "
checkJoin "/" = "/"
checkJoin α   = " " ◇ α ◇ " "

transformName ∷ ArtistResponse → Maybe Text
transformName = fromWords ◁ onWords (onTail checkCaps ◁ onLast filterTag) . name

transformArtistResponse ∷ ArtistResponse → Maybe Text
transformArtistResponse α =  (◇ (checkJoin $ join α)) ⊙ (transformName α)

transformArtists ∷ [ArtistResponse] → Maybe Text
transformArtists = smushWords ◁ traverse transformArtistResponse

transformAlbumArtist ∷ [ArtistResponse] → EyeD3Tag
transformAlbumArtist α = case transformArtists α of
  (Just "Various") → AlbumArtistParameter "Various Artists"
  (Just ω        ) → AlbumArtistParameter ω
  (Nothing       ) → AlbumArtistParameter ""

transformArtist ∷ [ArtistResponse] → EyeD3Tag
transformArtist = fromMaybe emptyArtist . (ArtistParameter ◁ transformArtists)
  where emptyArtist = ArtistParameter ""
