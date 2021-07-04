module Main where

import Prelude (Either, IO, String, ($), (>>=), pure, putStrLn)
import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.Aeson (eitherDecode)
import Datasource.Models.Arguments (files, flags, genre, parseArgs, url)
import Datasource.DiscogsRepository (fetch)
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
main = runProgram >>= putStrLn ||| pure
