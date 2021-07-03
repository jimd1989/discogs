module Main where

import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.Aeson (eitherDecode)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Tuple (uncurry)
import System.Process (system)
import Datasource.Arguments (expand, files, genre, parseArgs, url)
import Datasource.DiscogsRepository (fetch)
import Helpers ((◁), (◇))
import Output.Transformers.AlbumResponseTransformer (transformAlbum)

-- ID3 tagging takes place with external call to `eyeD3` for now
-- All errors are probably eyeD3's fault! Or string escaping.
-- Deal with exceptions as Either here?
runCmds ∷ [String] → [String] → IO ()
runCmds cmds files = traverse_ (uncurry runCmd) (zip cmds files)
  where runCmd cmd file = system (cmd ◇ " " ◇ file) $> ()

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args     ← ExceptT parseArgs
  response ← ExceptT $ fetch $ url args
  album    ← liftEither $ eitherDecode response
  cmds     ← pure $ transformAlbum (expand args) (genre args) album
  lift     $ runCmds cmds $ files args

main ∷ IO ()
main = runProgram >>= putStrLn ||| pure
