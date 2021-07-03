module Main where

import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.Aeson (eitherDecode)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Tuple (uncurry)
import System.Process (system)
import Datasource.DiscogsRepository (fetch)
import Output.Transformers.AlbumResponseTransformer (transformAlbum)
import Arguments (absolute, expand, files, flags, genre, parseArgs, url)
import Helpers ((◁), (◇))
import Parsing (decode', decode'')
import Processing (commands)

-- ID3 tagging takes place with external call to `eyeD3` for now
-- Deal with exceptions as Either here?
runCmds ∷ [String] → [String] → IO ()
runCmds cmds files = traverse_ (uncurry runCmd) (zip cmds files)
  where runCmd cmd file = system (cmd ◇ " " ◇ file) $> ()

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args     ← ExceptT parseArgs
  response ← ExceptT $ fetch $ url args
  album    ← liftEither $ eitherDecode response
  cmds     ← pure $ transformAlbum album
  lift $ putStrLn "placeholder"

main ∷ IO ()
main = runProgram >>= putStrLn ||| pure
