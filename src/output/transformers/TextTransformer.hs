module Output.Transformers.TextTransformer where

import Data.Char (toLower)
import Data.List (init, intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import Helpers ((◁), (◇), (⊙), fork, last')

lower ∷ Set String
lower = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To", "As"]

checkCaps ∷ String → String
checkCaps α = if member α lower then (map toLower α) else α

onLast ∷ (a → [a]) → [a] → Maybe [a]
onLast f α = ((init α ◇) . f) ⊙ (last' α)

onTail ∷ (a → a) → [a] → [a]
onTail f = fork (◇) (take 1) (f ◁ drop 1)

onWords ∷ ([String] → Maybe [String]) → String → Maybe String
onWords f = intercalate " " ◁ f . splitOn " "

--formatTitle ∷ Text → Text
--formatTitle = pack . onWords (onTail $ map checkCaps) . unpack
