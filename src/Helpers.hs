module Helpers ((◁), (◀), (⊙), (●), (◇), enumerate, fork, head', 
                iota, ix', last', putStderr, validate, wrap) where

import Prelude (Either, Int, IO, Maybe, String, (.), (<>), flip, pred)
import Control.Applicative (Applicative, (<*>), liftA2)
import Control.Error.Util (note)
import Control.Monad ((<=<))
import Data.Functor (($>), (<$>), fmap)
import Data.Ix (range)
import Data.List (length, zip)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (traverse)
import Data.Tuple (curry)
import System.IO (hPutStrLn, stderr)
import Safe (atMay)

fork :: Applicative f ⇒ (a → b → c) → f a → f b → f c
fork = liftA2

iota ∷ [a] → [Int]
iota = curry range 1 . length

enumerate ∷ [a] → [(a, Int)]
enumerate = zip ● iota

quote ∷ String → String
quote []       = []
quote ('\"':ω) = '\\' : '\"' : (quote ω)
quote (α:ω)    = α : (quote ω)

wrap ∷ String → String
wrap α = "\"" ◇ (quote α) ◇ "\""

ix' ∷ String → Int → [a] → Either String a
ix' α ω = note α . flip atMay ω

head' ∷ [a] → Maybe a
head' = flip atMay 0

last' ∷ [a] → Maybe a
last' = atMay ● (pred . length)

validate ∷ [a → Maybe ()] → a → Maybe a
validate α ω = traverse (\f → f ω) α $> ω

putStderr ∷ String → IO ()
putStderr = hPutStrLn stderr

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
