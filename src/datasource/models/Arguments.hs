module Datasource.Models.Arguments (Args(..), parseArgs) where

import Prelude (Either, IO, String, (.), ($), flip, not, pure)
import Control.Error.Util (note)
import Control.Monad (liftM4)
import Control.Monad.Except (ExceptT(..))
import Data.List (drop, elem, filter)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text, pack)
import System.Environment (getArgs)
import Datasource.Models.Flags (Flags, makeFlags, validFlags)
import Helpers ((◁), (⊙), ix', wrap)

data Args = Args {
  flags ∷ Flags,
  genre ∷ Text,
  url ∷ String, 
  files ∷ NonEmpty Text
}

makeFiles ∷ [String] → Either String (NonEmpty Text)
makeFiles = note "audio files not provided" . nonEmpty . (pack . wrap) ◁ drop 2

makeArgs ∷ [String] → Either String Args
makeArgs α = liftM4 Args (pure flags) genre url files
  where flags       = makeFlags α
        allButFlags = filter (not . flip elem validFlags) α
        genre       = pack ⊙ ix' "genre not provided" 0 allButFlags
        url         = ix' "url not provided" 1 allButFlags
        files       = makeFiles allButFlags

parseArgs ∷ ExceptT String IO Args
parseArgs = ExceptT $ makeArgs ⊙ getArgs
