module Main where

import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Tuple (uncurry)
import System.Environment (getArgs)
import System.Process (system)
import Fetching (fetch)
import Parsing (decode')
import Processing (commands)

runCmds ∷ [[Char]] → [[Char]] → IO ()
runCmds α ω = mapM_ (uncurry run) (zip α ω)
  where run α ω = system (α <> " " <> ω) $> ()

main :: IO ()
main = do
  args     ← getArgs
  absolute ← pure $ isJust $ find ((== "-a")) args
  rest     ← pure $ filter (/= "-a") args
  genre    ← pure $ rest !! 0
  url      ← pure $ rest !! 1
  files    ← pure $ tail $ tail rest
  album    ← decode' <$> fetch url
  cmds     ← pure $ (commands genre) <$> album
  _        ← case cmds of
    Nothing → pure ()
    Just α  → mapM_ putStrLn α
  return ()
