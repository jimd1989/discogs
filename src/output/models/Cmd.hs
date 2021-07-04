module Output.Models.Cmd (Cmd(..), cmd) where

import Prelude (Bool, IO, Either, String, (.), ($), flip, show)
import Control.Error.Util (note)
import System.Directory (findExecutable)
import Helpers ((◁), (⊙), (◇))

data Cmd = Cmd {
  essential ∷ Bool,
  name ∷ Either String String,
  args ∷ String
}

cmd ∷ Bool → String → String → IO Cmd
cmd needed cmdName ars = makeCmd ⊙ (show ◁ toEither) ⊙ findExecutable cmdName
  where toEither = note ("Command not found: " ◇ cmdName)
        makeCmd  = (flip $ Cmd needed) ars
