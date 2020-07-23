module Main where

import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Tuple (uncurry)
import System.Environment (getArgs)
import System.Process (system)
import Fetching (fetch)
import Helpers (wrap)
import Parsing (decode')
import Processing (commands)

runCmds ∷ [[Char]] → [[Char]] → IO ()
runCmds α ω = mapM_ (uncurry run) (zip α ω)
  where run α ω = system (α <> " " <> ω) $> ()

main :: IO ()
main = do
  args     ← getArgs
  absolute ← pure $ isJust $ find (== "-a") args
  expand   ← pure $ isJust $ find (== "-e") args
  rest     ← pure $ filter (not . flip elem ["-a", "-e"]) args
  genre    ← pure $ rest !! 0
  url      ← pure $ rest !! 1
  files    ← pure $ map wrap $ tail $ tail rest
  album    ← (decode' expand) <$> fetch url
  cmds     ← pure $ (commands genre absolute) <$> album
  _        ← case cmds of
    Nothing → pure ()
    Just α  → runCmds α files
  return ()
