module Helpers where

import Control.Error.Util (note)
import Control.Monad ((<=<), liftM2)
import Data.Function (flip)
import Data.Ix (range)
import Data.Maybe (Maybe)
import Data.List (tails)
import Data.Tuple (curry)
import Safe (atMay, atNote, tailMay)

dyfork ∷ (Monad m, Monad n) ⇒ (a → b → c) → m (n a) → m (n b) → m (n c)
dyfork = liftM2 . liftM2

run ∷ [a] → [Int]
run = curry range 1 . length

enumerate ∷ [a] → [(Int, a)]
enumerate = flip zip <*> run

quote ∷ String → String
quote []       = []
quote ('\"':ω) = '\\' : '\"' : (quote ω)
quote (α:ω)    = α : (quote ω)

wrap ∷ String → String
wrap α = "\"" <> (quote α) <> "\""

fst' ∷ (a, b, c) → a
fst' (α, _, _) = α

snd' ∷ (a, b, c) → b
snd' (_, α, _) = α

thd' ∷ (a, b, c) → c
thd' (_, _, α) = α


safeIx ∷ String → Int → [a] → Either String a
safeIx α ω = note α . flip atMay ω

safeDrop ∷ String → Int → [a] → Either String [a]
safeDrop α ω = safeIx α ω . tails

f ◁ g = fmap f . g
infixr 9 ◁

f ◀ g = f <=< g
infixr 1 ◀

