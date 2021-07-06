module Main where

import Prelude (Either, IO, String, ($), (>>=), pure)
import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import Datasource.Models.Arguments (Args(..), parseArgs)
import Datasource.DiscogsRepository (fetch)
import Helpers (putStderr)
import Output.Execute (executeCmds)
import Output.Transformers.AlbumResponseTransformer (transformAlbum)

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args      ← ExceptT parseArgs
  response  ← ExceptT $ fetch (url args)
  album     ← liftEither $ eitherDecode response
  eyeD3Args ← liftEither $ transformAlbum (flags args) (genre args) album
  executeCmds eyeD3Args (files args)

main ∷ IO ()
main = runProgram >>= putStderr ||| pure
