module Datasource.Models.Arguments (Args(..), parseArgs) where

import Control.Error.Util (note)
import Control.Monad (liftM4)
import Prelude (Either, IO, String, (.), flip, not, pure)
import Data.List (drop, elem, filter)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import System.Environment (getArgs)
import Datasource.Models.Flags (Flags, makeFlags, validFlags)
import Helpers ((◁), (⊙), ix', wrap)

data Args = Args {
  flags ∷ Flags,
  genre ∷ String,
  url ∷ String, 
  files ∷ NonEmpty String
}

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
