module Debug (debug, defaultFlags) where

import Prelude (Either(..), Maybe(..), String, ($), (>>=))
import Control.Monad.Except (runExceptT)
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson (eitherDecode)
import Datasource.DiscogsRepository (fetch)
import Datasource.Models.AlbumResponse (AlbumResponse(..))
import Datasource.Models.Flags (Flags, makeFlags)

debug ∷ String → AlbumResponse
debug α = case (unsafePerformIO (runExceptT $ fetch α) >>= eitherDecode) of
  (Right α) → α
  (Left  _) → AlbumResponse [] "" [] Nothing

defaultFlags ∷ Flags
defaultFlags = makeFlags []
