module Output.Execute (executeCmds) where

import Prelude (Bool(..), Either(..), IO, String, (.), ($), (*>), pure, show)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty, zipWith)
import Data.Text (Text, unpack)
import System.IO (stderr, stdout)
import System.IO.Silently (hSilence)
import System.Process (system)
import Output.Models.Cmd (Cmd(..), CmdName(..), cmd)
import Helpers ((◇), putStderr)

runOnFiles ∷ MonadIO m ⇒ String → NonEmpty Text → m ()
runOnFiles f args = liftIO $ shush $ mapConcurrently_ execute args
  where shush   = hSilence [stderr, stdout]
        execute = system . (f ◇) . unpack

run ∷ (MonadError String m, MonadIO m) ⇒ Cmd → NonEmpty Text → m ()
run α args =
  let fullCmd = show α
  in case (essential α, executable α) of
    (False, Left(_) ) → pure ()
    (True,  Left(ω) ) → throwError(ω)
    (_   ,  Right(_)) → runOnFiles fullCmd args $> ()

executeCmds ∷ (MonadError String m, MonadIO m) ⇒ 
              NonEmpty Text → NonEmpty Text → m ()
executeCmds eyeD3Args files = do
  mp3val    ← cmd Mp3Val
  eyeD3     ← cmd EyeD3
  argsFiles ← pure $ zipWith (\α ω → α ◇ " " ◇ ω) eyeD3Args files
  _         ← putStderr "Tagging files"
  _         ← run mp3val files *> run eyeD3 argsFiles
  traverse_ (putStderr . unpack) argsFiles
