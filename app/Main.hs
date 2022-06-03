module Main where

import Prelude (IO, String, ($), (>>=), pure)
import Control.Arrow ((|||))
import Control.Monad.Except (MonadError, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (eitherDecode)
import Datasource.Models.Arguments (Args(..), parseArgs)
import Datasource.DiscogsRepository (fetch)
import Helpers (putStderr)
import Output.Execute (executeCmds)
import Output.Transformers.AlbumResponseTransformer (transformAlbum)

program ∷ (MonadError String m, MonadIO m) ⇒ m ()
program = do
  args      ← parseArgs
  response  ← fetch (url args)
  album     ← liftEither $ eitherDecode response
  eyeD3Args ← transformAlbum (flags args) (genre args) album
  executeCmds eyeD3Args (files args)

main ∷ IO ()
main = runExceptT program >>= putStderr ||| pure
