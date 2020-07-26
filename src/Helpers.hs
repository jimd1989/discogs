module Helpers where

import Control.Monad (liftM2)
import Data.Function (flip)
import Data.Ix (range)
import Data.Tuple (curry)

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
