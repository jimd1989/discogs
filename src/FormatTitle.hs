module FormatTitle where

import Control.Monad (liftM2)
import Data.Char (toLower)
import Data.List (head, intercalate, last, tail)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import Data.Text (Text, pack, unpack)

tag ∷ Set Char
tag = fromList "(0123456789)"

allTag ∷ [Char] → Bool
allTag = and . map (flip member tag)

brackets ∷ [Char] → Bool
brackets = liftM2 (&&) ((== '(') . head) ((== ')') . last)

isTag ∷ [Char] → Bool
isTag = liftM2 (&&) brackets allTag

checkTag ∷ [Char] → [Char]
checkTag α = if isTag α then "" else α

lower ∷ Set [Char]
lower = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To"]

checkCaps ∷ [Char] → [Char]
checkCaps α = if (member α lower) then (map toLower α) else α

checkJoin ∷ [Char] → [Char]
checkJoin "" = ""
checkJoin α  = " " <> α <> " "

onWords ∷ ([[Char]] → [[Char]]) → [Char] → [Char]
onWords f = intercalate " " . filter (/= "") . f . splitOn " "

onRest ∷ ([[Char]] → [[Char]]) → [[Char]] → [[Char]]
onRest f = liftM2 (:) head (f . tail)

formatArtist ∷ Text → Text → Text
formatArtist α ω = (artist α) <> (joiner ω)
  where artist = pack . onWords (onRest (map checkCaps) . map checkTag) . unpack
        joiner = pack . checkJoin . onWords (map checkCaps) . unpack

formatTitle ∷ Text → Text
formatTitle = pack . onWords (onRest $ map checkCaps) . unpack
