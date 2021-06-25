module Arguments where

import Control.Monad (liftM4, liftM2)
import Data.List (elem)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Helpers ((◁), (⊙), fork, safeIx, safeDrop)

data Flags = Flags { absolute ∷ Bool, split ∷ Bool }

data Args = Args {
  flags ∷ Flags,
  genre ∷ String,
  url ∷ String, 
  files ∷ [String]
}

validFlags ∷ [String]
validFlags = ["-a", "-e"]

makeFlags ∷ [String] → Flags
makeFlags = fork Flags (elem "-a") (elem "-e")

makeArgs ∷ [String] → Either String Args
makeArgs α = liftM4 Args (pure flags) url genre files
  where flags       = makeFlags α
        allButFlags = filter (not . flip elem validFlags) α
        url         = safeIx "url not provided" 1 allButFlags
        genre       = safeIx "genre not provided" 2 allButFlags
        files       = safeDrop "files not provided" 2 allButFlags

parseArgs ∷ IO (Either String Args)
parseArgs = makeArgs ⊙ getArgs
