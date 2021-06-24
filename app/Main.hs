module Main where

import Control.Applicative (liftA3)
import Control.Arrow (left)
import Control.Exception (displayException, try)
import Control.Error.Util (note)
import Control.Monad ((<=<), join, liftM2)
import Data.ByteString.Lazy (ByteString)
import Data.Function (const)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Tuple (uncurry)
import GHC.IO.Exception (IOException)
import Safe (atMay, atNote, tailMay)
import System.Environment (getArgs)
import System.Process (system)
import Fetching (fetch)
import Helpers ((◁), safeIx, safeDrop, wrap)
import Parsing (Album, decode')
import Processing (commands)

type Args = [[Char]]

try' ∷ String → IO a → IO (Either String a)
try' α = left (const α ∷ IOException → String) ◁ try

flag ∷ [Char] → Args → Bool
flag α = isJust . find (== α)

-- Move to Fetching?
getInfo ∷ Args → IO (Either String ByteString)
getInfo = join ◁ sequence . (try' msg . fetch) ◁ url
  where url = safeIx "URL not provided" 1
        msg = "error fetching from Discogs"

-- Need data structure for all args
getCmds ∷ Args → Bool → Either String Album → Either String [String]
getCmds α absolute ω = liftA3 commands genre (pure absolute) ω
  where genre = safeIx "genre not provided" 0 α

getFiles ∷ Args → Either String [String]
getFiles = wrapInQuotes ◁ getFileArgs
  where getFileArgs  = safeDrop "files not provided" 2
        wrapInQuotes = map wrap

runCmds ∷ [String] → [String] → IO ()
runCmds α ω = mapM_ (uncurry run) (zip α ω)
  where run α ω = system (α <> " " <> ω) $> ()

-- Awful! Rewrite
main ∷ IO ()
main = do
  args  ← getArgs
  rest  ← pure $ filter (not . flip elem ["-a", "-e"]) args
  info  ← getInfo rest
  album ← pure $ info >>= decode' (flag "-e" args)
  cmds  ← pure $ getCmds rest (flag "-a" args) album
  files ← pure $ getFiles rest
  exec  ← pure $ liftM2 runCmds cmds files
  case exec of
    Right α → α
    Left  ω → putStrLn ω
