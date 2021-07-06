module Output.Transformers.TextTransformer
  (checkCaps, fromWords, onLast, onTail, onWords, smushWords, transformText)
where

import Prelude (Maybe, String, (.))
import Data.List (drop, init, take)
import Data.Set (Set, fromList, member)
import Data.Text (Text, intercalate, splitOn, toLower)
import Helpers ((◁), (◇), (⊙), fork, last')

lower ∷ Set Text
lower = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To", "As"]

checkCaps ∷ Text → Text
checkCaps α = if member α lower then toLower α else α

onLast ∷ (a → [a]) → [a] → Maybe [a]
onLast f α = ((init α ◇) . f) ⊙ (last' α)

onTail ∷ (a → a) → [a] → [a]
onTail f = fork (◇) (take 1) (f ◁ drop 1)

onWords ∷ ([Text] → a) → Text → a
onWords f = f . splitOn " "

fromWords ∷ [Text] → Text
fromWords = intercalate " "

smushWords ∷ [Text] → Text
smushWords = intercalate ""

transformText ∷ Text → Text
transformText = fromWords . onWords (onTail checkCaps)
