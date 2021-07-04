module Datasource.Models.Arguments where

import Control.Error.Util (note)
import Control.Monad (liftM4)
import Data.List (drop, elem)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import System.Environment (getArgs)
import Helpers ((◁), (⊙), fork, ix', wrap)

data Flags = Flags {
  absolute ∷ Bool, 
  expand ∷ Bool 
}

data Args = Args {
  flags ∷ Flags,
  genre ∷ String,
  url ∷ String, 
  files ∷ NonEmpty String
}

validFlags ∷ [String]
validFlags = ["-a", "-e"]

makeFlags ∷ [String] → Flags
makeFlags = fork Flags (elem "-a") (elem "-e")

makeFiles ∷ [String] → Either String (NonEmpty String)
makeFiles = note "audio files not provided" . nonEmpty . wrap ◁ drop 2

makeArgs ∷ [String] → Either String Args
makeArgs α = liftM4 Args (pure flags) url genre files
  where flags       = makeFlags α
        allButFlags = filter (not . flip elem validFlags) α
        url         = ix' "url not provided" 0 allButFlags
        genre       = ix' "genre not provided" 1 allButFlags
        files       = makeFiles allButFlags

parseArgs ∷ IO (Either String Args)
parseArgs = makeArgs ⊙ getArgs
