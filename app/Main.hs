module Main where

import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Tuple (uncurry)
import System.Process (system)
import Arguments (absolute, expand, files, flags, genre, parseArgs, url)
import Fetching (fetch)
import Helpers ((◁), (◇))
import Parsing (decode')
import Processing (commands)

-- ID3 tagging takes place with external call to `eyeD3` for now
runCmds ∷ [String] → [String] → IO ()
runCmds cmds files = traverse_ (uncurry runCmd) (zip cmds files)
  where runCmd cmd file = system (cmd ◇ " " ◇ file) $> ()

runProgram ∷ IO (Either String ())
runProgram = runExceptT $ do
  args     ← ExceptT parseArgs
  response ← ExceptT $ fetch $ url args
  album    ← liftEither $ decode' (expand $ flags args) response
  cmds     ← pure $ commands (genre args) (absolute $ flags args) album
  lift     $ runCmds cmds (files args)

main ∷ IO ()
main = runProgram >>= putStrLn ||| pure
