module Output.Models.Cmd (Cmd(..), cmd) where

import Prelude (Bool, IO, Either, String, (.), ($), flip, show)
import Control.Error.Util (note)
import System.Directory (findExecutable)
import Helpers ((◁), (⊙), (◇))

data Cmd = Cmd {
  essential ∷ Bool,
  runCmd ∷ Either String String
}

cmd ∷ String → String → Bool → IO Cmd
cmd cmdName args needed = Cmd needed ⊙ (annotate ◁ convert) ⊙ find
  where find       = findExecutable cmdName
        convert    = note ("Command not found: " ◇ cmdName)
        annotate α = α ◇ " " ◇ args ◇ " "
