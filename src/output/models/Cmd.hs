module Output.Models.Cmd (Cmd(..), CmdName(..), cmd) where

import Prelude (Bool(..), IO, Either(..), Show, String, (.), ($), flip, show)
import Control.Error.Util (note)
import System.Directory (findExecutable)
import Helpers ((◁), (⊙), (◇), fork)

data CmdName = Mp3Val | EyeD3

instance Show CmdName where
  show Mp3Val = "mp3val"
  show EyeD3  = "eyeD3"

data Cmd = Cmd {
  essential ∷ Bool,
  cmdName ∷ CmdName,
  executable ∷ Either String String
}

instance Show Cmd where
  show α = case (cmdName α, executable α) of
    (Mp3Val, Right(ω)) → ω ◇ " -f -nb "
    (EyeD3,  Right(ω)) → ω ◇ " "
    (_    ,  _       ) → ""

cmd ∷ CmdName → IO Cmd
cmd name =
  let needed  = case name of
                Mp3Val → False
                EyeD3  → True
      find    = findExecutable $ show name
      convert = note ("Command not found: " ◇ (show name))
      make    = Cmd needed name
  in (make ⊙ convert) ⊙ find
