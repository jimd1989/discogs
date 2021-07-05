module Output.Execute (executeCmds) where

import Prelude (Bool(..), Either(..), IO, String, (.), ($), (*>), pure, show)
import Control.Monad.Except (ExceptT(..), lift)
import Data.Functor (($>))
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty, zipWith)
import System.Process (system)
import Output.Models.Cmd (Cmd(..), CmdName(..), cmd)
import Helpers ((◇))

runOnFiles ∷ String → NonEmpty String → IO ()
runOnFiles f args = traverse_ (system . (f ◇)) args

run ∷ Cmd → NonEmpty String → IO (Either String ())
run α args =
  let fullCmd = show α
  in case (essential' α, executable α) of
    (False, Left(_))  → pure $ pure ()
    (True,  Left(ω))  → pure $ Left(ω)
    (_   ,  Right(_)) → runOnFiles fullCmd args $> pure ()

executeCmds ∷ NonEmpty String → NonEmpty String → ExceptT String IO ()
executeCmds eyeD3Args files = do
  mp3val    ← lift $ cmd Mp3Val
  eyeD3     ← lift $ cmd EyeD3
  argsFiles ← pure $ zipWith (\α ω → α ◇ " " ◇ ω) eyeD3Args files
  ExceptT   $ run mp3val files *> run eyeD3 argsFiles
