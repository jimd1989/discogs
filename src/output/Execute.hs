module Output.Execute where

import Prelude (Bool(..), Either(..), IO, String, ($), pure)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe(..))
import Output.Models.Cmd (Cmd(..), cmd)

eyeD3 ∷ IO Cmd
eyeD3 = cmd True "eyeD3" ""

mp3Val ∷ IO Cmd
mp3Val = cmd False "mp3val" "-f"

--run ∷ Cmd → NonEmpty String → IO (Either String ())
--run α files = case (essential α, name α) of
--  (False, Nothing) → pure $ pure ()
  --(True,  Nothing) → pure $ Left "
