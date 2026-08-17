module Datasource.Models.Flags (Flags(..), makeFlags, validFlags) where

import Prelude (Bool, String, elem)
import Helpers (fork)

data Flags = Flags {
  absolute ∷ Bool, 
  expand ∷ Bool 
}

validFlags ∷ [String]
validFlags = ["-a", "-e"]

makeFlags ∷ [String] → Flags
makeFlags = fork Flags (elem "-a") (elem "-e")
