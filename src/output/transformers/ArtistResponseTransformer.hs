module Output.Transformers.ArtistResponseTransformer where

import Data.List (head, last, tail)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import Data.Text (Text, pack, unpack)
import Helpers ((◇), (≠), fork, safeIx)
import Output.Models.EyeD3Parameter (EyeD3Parameter)

tag ∷ Set Char
tag = fromList "(0123456789)"

isAllInTag ∷ String → Bool
isAllInTag = and . map (flip member tag)

-- Head is technically unsafe here, as is tail/last ?

isBracketed ∷ String → Bool
isBracketed = fork (&&) ((== '(') . head) ((== ')') . last)

isTag ∷ String → Bool
isTag = fork (&&) isBracketed isAllInTag

checkTag ∷ String → String
checkTag α = if isTag α then "" else α

checkJoin ∷ String → String
checkJoin ""  = ""
checkJoin "," = ", "
checkJoin α   = " " ◇ α ◇ " "

--formatArtist ∷ Text → Text → Text
--formatArtist α ω = (artist α) ◇ (joiner ω)
--  where artist = pack . onWords (onTail (map checkCaps) . map checkTag) . unpack
--        joiner = pack . checkJoin . onWords (map checkCaps) . unpack

--transformArtists ∷ [ArtistsResponse] → EyeD3Parameter
