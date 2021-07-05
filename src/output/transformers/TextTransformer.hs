module Output.Transformers.TextTransformer (checkCaps, fromWords, onLast, 
                                            onTail, onWords, transformText,
checkCaps', fromWords', onWords', transformText')
where

import Prelude (Maybe, String, (.))
import Data.Char (toLower)
import Data.List (drop, init, intercalate, take)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import qualified Data.Text as T
import Helpers ((◁), (◇), (⊙), fork, last')

lower ∷ Set String
lower = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To", "As"]

checkCaps ∷ String → String
checkCaps α = if member α lower then (toLower ⊙ α) else α

onWords ∷ ([String] → a) → String → a
onWords f = f . splitOn " "

fromWords ∷ [String] → String
fromWords = intercalate " "

transformText ∷ String → String
transformText = fromWords . onWords (onTail checkCaps)

-- new
lower' ∷ Set T.Text
lower' = fromList ["A", "An", "And", "By", "In", "On", "Of", "At", "With", "The",
                  "For", "From", "Into", "Unto", "To", "As"]

checkCaps' ∷ T.Text → T.Text
checkCaps' α = if member α lower' then T.toLower α else α

onLast ∷ (a → [a]) → [a] → Maybe [a]
onLast f α = ((init α ◇) . f) ⊙ (last' α)

onTail ∷ (a → a) → [a] → [a]
onTail f = fork (◇) (take 1) (f ◁ drop 1)

onWords' ∷ ([T.Text] → a) → T.Text → a
onWords' f = f . T.splitOn " "

fromWords' ∷ [T.Text] → T.Text
fromWords' = T.intercalate " "

transformText' ∷ T.Text → T.Text
transformText' = fromWords' . onWords' (onTail checkCaps')
