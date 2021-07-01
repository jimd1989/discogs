module Output.Transformers.TextTransformer where

import Data.Char (toLower)
import Data.List (head, intercalate, last, tail)
import Data.List.Split (splitOn)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set, fromList, member)
import Data.Text (Text, pack, unpack)
import Helpers ((◁), (◇), (≠), fork, safeSplit)
import Output.Models.EyeD3Parameter (EyeD3Parameter)

tag ∷ Set Char
tag = fromList "(0123456789)"

isAllInTag ∷ String → Bool
isAllInTag = and . map (flip member tag)

-- Head is technically unsafe here
isBracketed ∷ String → Bool
isBracketed = fork (&&) ((== '(') . head) ((== ')') . last)

isTag ∷ String → Bool
isTag = fork (&&) isBracketed isAllInTag

checkTag ∷ String → String
checkTag α = if isTag α then "" else α

lower ∷ Set String
lower = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To", "As"]

checkCaps ∷ String → String
checkCaps α = if (member α lower) then (map toLower α) else α

checkJoin ∷ String → String
checkJoin ""  = ""
checkJoin "," = ", "
checkJoin α   = " " ◇ α ◇ " "

onWords ∷ ([String] → [String]) → String → String
onWords f = intercalate " " . filter (≠ "") . f . splitOn " "

onTail ∷ ([String] → [String]) → [String] → [String]
onTail f = fork (:) head (f . tail)

formatArtist ∷ Text → Text → Text
formatArtist α ω = (artist α) ◇ (joiner ω)
  where artist = pack . onWords (onTail (map checkCaps) . map checkTag) . unpack
        joiner = pack . checkJoin . onWords (map checkCaps) . unpack

formatTitle ∷ Text → Text
formatTitle = pack . onWords (onTail $ map checkCaps) . unpack

-- new

isAllInTag' ∷ NonEmpty Char → Bool
isAllInTag' = and . flip member tag ◁ NE.toList 

isBracketed' ∷ NonEmpty Char → Bool
isBracketed' = fork (&&) ((== '(') . NE.head) ((== ')') . NE.last)

isTag' ∷ NonEmpty Char → Bool
isTag' = fork (&&) isBracketed' isAllInTag'

checkTag' ∷ NonEmpty Char → String
checkTag' α = if isTag' α then "" else NE.toList α

-- On words should turn its SPLIT strings to nonempty after filtering
-- but the list itself can be nullable
-- onTail just needs an empty list case

-- ReWrite ONTAIL

splitWordsWith ∷ (NonEmpty Char → String) → String → [String]
splitWordsWith f = filter (≠ "") . f ◁ NE.fromList ◁ filter (≠ "") . splitOn " "

onTail' ∷ (String → String) → [String] → [String]
onTail' f []    = []
onTail' f (α:ω) = α : (map f ω)


-- NE type of checkCaps?
-- Fuck it, just use Either/Maybe
f = g
  where g = onTail' checkCaps . splitWordsWith checkTag'

--f = onWords' (onTail' checkCaps . map checkTag')


transformText ∷ (String → EyeD3Parameter) → String → EyeD3Parameter
transformText f = f . onWords (onTail $ map checkCaps)

formatTitle' ∷ Text → String
formatTitle' = onWords (onTail $ map checkCaps) . unpack
