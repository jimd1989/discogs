module Helpers where

import Control.Monad (liftM2)
import Data.Function (flip)
import Data.Ix (range)
import Data.Maybe (Maybe(..))
import Data.Tuple (curry)

dyfork ∷ (Monad m, Monad n) ⇒ (a → b → c) → m (n a) → m (n b) → m (n c)
dyfork = liftM2 . liftM2

maybeIf ∷ (a → Bool) → a → Maybe a
maybeIf f α | f α       = Just α
            | otherwise = Nothing

enumerate ∷ [a] → [(Int, a)]
enumerate = flip zip <*> (curry range 1 . length)
