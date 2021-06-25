module Helpers where

import Control.Applicative (liftA2)
import Control.Error.Util (note)
import Control.Monad ((<=<))
import Data.Function (flip)
import Data.Ix (range)
import Data.List (intercalate, tails)
import Data.Tuple (curry)
import Safe (atMay)

fork :: Applicative f ⇒ (a → b → c) → f a → f b → f c
fork = liftA2

dyfork ∷ (Applicative m, Applicative n) ⇒ 
         (a → b → c) → m (n a) → m (n b) → m (n c)
dyfork = fork . fork

run ∷ [a] → [Int]
run = curry range 1 . length

enumerate ∷ [a] → [(Int, a)]
enumerate = flip zip ● run

quote ∷ String → String
quote []       = []
quote ('\"':ω) = '\\' : '\"' : (quote ω)
quote (α:ω)    = α : (quote ω)

wrap ∷ String → String
wrap α = "\"" <> (quote α) <> "\""

spaceOut ∷ [String] → String
spaceOut = intercalate " "

-- Can this tuples be avoided?
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

-- Digraph Tl
f ◁ g = fmap f . g
infixr 9 ◁

-- Digraph PL
f ◀ g = f <=< g
infixr 1 ◀

-- Digraph 0.
f ⊙ g = f <$> g
infixl 4 ⊙

-- Digraph 0M
f ● g = f <*> g
infixl 4 ●

-- Digraph Dw
α ◇ ω = α <> ω
infixr 5 ◇
