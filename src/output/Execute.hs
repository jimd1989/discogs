module Output.Execute (executeCmds) where

import Prelude (Bool(..), Either(..), IO, String, (.), ($), pure)
import Control.Monad.Except (ExceptT(..), lift)
import Data.Functor (($>))
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty, zipWith)
import System.Process (system)
import Output.Models.Cmd (Cmd(..), cmd)
import Helpers ((◇))

-- Doesn't raise exceptions from `system`.
-- One bad operation shouldn't stop the rest.
run ∷ Cmd → NonEmpty String → IO (Either String ())
run α args = case (essential α, runCmd α) of
  (False, Left(_) ) → pure $ pure ()
  (True,  Left(ω) ) → pure $ Left(ω)
  (_,     Right(ω)) → traverse_ (system . (ω ◇)) args $> pure ()

executeCmds ∷ NonEmpty String → NonEmpty String → ExceptT String IO ()
executeCmds eyeD3Args files = do
  mp3val  ← lift $ cmd "mp3val" "-f" False
  _       ← ExceptT $ run mp3val files
  eyeD3   ← lift $ cmd "eyeD3" "" True
  ExceptT $ run eyeD3 (zipWith (\α ω → α ◇ " " ◇ ω) eyeD3Args files)
