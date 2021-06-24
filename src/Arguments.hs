module Arguments where

import Control.Monad (liftM4)
import Data.List (elem)
import Data.List.Split (splitOn)
import Helpers (safeIx, safeDrop)

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
makeFlags α = Flags { absolute = elem "-a" α, split = elem "-e" α }

parseArgs ∷ String → Either String Args
parseArgs α = liftM4 Args (pure flags) url genre files
  where splitArgs   = splitOn " " α
        flags       = makeFlags splitArgs
        allButFlags = filter (not . flip elem validFlags) splitArgs
        url         = safeIx "url not provided" 1 allButFlags
        genre       = safeIx "genre not provided" 2 allButFlags
        files       = safeDrop "files not provided" 2 allButFlags
