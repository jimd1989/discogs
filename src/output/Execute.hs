module Output.Execute (executeCmds) where

import Prelude (Bool(..), Either(..), IO, String, (.), ($), (*>), pure, show)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Except (ExceptT(..), lift)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty, zipWith)
import Data.Text (Text, unpack)
import System.IO (stderr, stdout)
import System.IO.Silently (hSilence)
import System.Process (system)
import Output.Models.Cmd (Cmd(..), CmdName(..), cmd)
import Helpers ((◇), putStderr)

runOnFiles ∷ String → NonEmpty Text → IO ()
runOnFiles f args = hSilence [stderr, stdout] $ mapConcurrently_ execute args
  where execute = system . (f ◇) . unpack

run ∷ Cmd → NonEmpty Text → IO (Either String ())
run α args =
  let fullCmd = show α
  in case (essential α, executable α) of
    (False, Left(_) ) → pure $ pure ()
    (True,  Left(ω) ) → pure $ Left(ω)
    (_   ,  Right(_)) → runOnFiles fullCmd args $> pure ()

executeCmds ∷ NonEmpty Text → NonEmpty Text → ExceptT String IO ()
executeCmds eyeD3Args files = do
  mp3val    ← lift $ cmd Mp3Val
  eyeD3     ← lift $ cmd EyeD3
  argsFiles ← pure $ zipWith (\α ω → α ◇ " " ◇ ω) eyeD3Args files
  _         ← lift $ putStderr "Tagging files"
  _         ← ExceptT $ run mp3val files *> run eyeD3 argsFiles
  lift      $ traverse_ (putStderr . unpack) argsFiles
