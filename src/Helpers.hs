module Helpers where

import Control.Applicative (liftA2)
import Control.Error.Util (hush, note)
import Control.Monad ((<=<))
import Data.Function (flip)
import Data.Functor (($>))
import Data.Ix (range)
import Data.List (intercalate, tails)
import Data.List.Split (splitOn)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (curry)
import Safe (atMay)

fork :: Applicative f ⇒ (a → b → c) → f a → f b → f c
fork = liftA2

dyfork ∷ (Applicative m, Applicative n) ⇒ 
         (a → b → c) → m (n a) → m (n b) → m (n c)
dyfork = fork . fork

indices ∷ [a] → [Int]
indices = curry range 1 . length

enumerate' ∷ [a] → [(Int, a)]
enumerate' = flip zip ● indices

enumerate ∷ [a] → [(a, Int)]
enumerate = zip ● indices

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

safeSplit ∷ Eq a ⇒ [a] → [a] → NonEmpty [a]
safeSplit α ω = fromMaybe (NE.fromList [ω]) (NE.nonEmpty $ splitOn α ω)

safeHead ∷ NonEmpty a → a
safeHead = NE.head

safeLast ∷ NonEmpty a → a
safeLast = NE.last

head' ∷ [a] → Maybe a
head' = hush . safeIx "" 0

last' ∷ [a] → Maybe a
last' α = hush $ safeIx "" (pred $ length α) α

validate ∷ [a → Maybe ()] → a → Maybe a
validate α ω = traverse (\f → f ω) α $> ω

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

-- Digraph !=
α ≠ ω = α /= ω
infix 4 ≠
