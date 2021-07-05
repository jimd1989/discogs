module Output.Execute (executeCmds) where

import Prelude (Bool(..), Either(..), IO, String, ($), (*>), pure, show)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Except (ExceptT(..), lift)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty, zipWith)
import System.IO (stderr, stdout)
import System.IO.Silently (hSilence)
import System.Process (system)
import Output.Models.Cmd (Cmd(..), CmdName(..), cmd)
import Helpers ((◇), putStderr)

runOnFiles ∷ String → NonEmpty String → IO ()
runOnFiles f args = hSilence [stderr, stdout] $ mapConcurrently_ execute args
  where oneCmd α  = f ◇ α
        print α   = putStderr (oneCmd α)
        execute α = system $ oneCmd α

run ∷ Cmd → NonEmpty String → IO (Either String ())
run α args =
  let fullCmd = show α
  in case (essential' α, executable α) of
    (False, Left(_) ) → pure $ pure ()
    (True,  Left(ω) ) → pure $ Left(ω)
    (_   ,  Right(_)) → runOnFiles fullCmd args $> pure ()

executeCmds ∷ NonEmpty String → NonEmpty String → ExceptT String IO ()
executeCmds eyeD3Args files = do
  mp3val    ← lift $ cmd Mp3Val
  eyeD3     ← lift $ cmd EyeD3
  argsFiles ← pure $ zipWith (\α ω → α ◇ " " ◇ ω) eyeD3Args files
  _         ← lift $ traverse_ putStderr argsFiles
  ExceptT   $ run mp3val files *> run eyeD3 argsFiles
