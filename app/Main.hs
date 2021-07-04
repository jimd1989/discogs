module Main where

import Prelude (Either, IO, String, ($), (>>=), pure, putStrLn)
import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.Aeson (eitherDecode)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty, zip)
import Data.Tuple (uncurry)
import System.Process (system)
import Datasource.Models.Arguments (files, flags, genre, parseArgs, url)
import Datasource.DiscogsRepository (fetch)
import Helpers ((◁), (◇))
import Output.Transformers.AlbumResponseTransformer (transformAlbum)

-- ID3 tagging takes place with external call to `eyeD3` for now
-- All errors are probably eyeD3's fault! Or string escaping.
runCmds ∷ NonEmpty String → NonEmpty String → IO ()
runCmds cmds files = traverse_ (uncurry runCmd) (zip cmds files)
  where runCmd cmd file = system (cmd ◇ " " ◇ file) $> ()

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args     ← ExceptT parseArgs
  response ← ExceptT $ fetch $ url args
  album    ← liftEither $ eitherDecode response
  cmds     ← liftEither $ transformAlbum (flags args) (genre args) album
  lift     $ runCmds cmds $ files args

main ∷ IO ()
main = runProgram >>= putStrLn ||| pure
