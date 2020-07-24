module Main where

import Control.Arrow (left)
import Control.Exception (displayException, try)
import Control.Error.Util (note)
import Control.Monad ((<=<), join, liftM2)
import Data.Function (const)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Tuple (uncurry)
import GHC.IO.Exception (IOException)
import Safe (atMay, tailMay)
import System.Environment (getArgs)
import System.Process (system)
import Fetching (fetch)
import Helpers (wrap)
import Parsing (Album, decode')
import Processing (commands)

type Args = [[Char]]

try' ∷ String → IO a → IO (Either String a)
try' α = fmap (left (const α ∷ IOException → String)) . try

flag ∷ [Char] → Args → Bool
flag α = isJust . find (== α)

getInfo ∷ Args → IO (Either String String)
getInfo = fmap join . sequence . fmap (try' msg . fetch) . url
  where url = note "valid URL not provided" . flip atMay 1
        msg = "error fetching from Discogs"

getCmds ∷ Args → Bool → Either String Album → Either String [String]
getCmds α absolute ω = commands <$> genre <*> (pure absolute) <*> ω
  where genre = note "valid genre not provided" $ atMay α 0

getFiles ∷ Args → Either String [String]
getFiles = fmap (map wrap) . note "error getting files" . (tailMay <=< tailMay)

runCmds ∷ [String] → [String] → IO ()
runCmds α ω = mapM_ (uncurry run) (zip α ω)
  where run α ω = system (α <> " " <> ω) $> ()

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

--main :: IO ()
--main = do
--  args     ← getArgs
--  absolute ← pure $ isJust $ find (== "-a") args
--  expand   ← pure $ isJust $ find (== "-e") args
--  rest     ← pure $ filter (not . flip elem ["-a", "-e"]) args
--  genre    ← pure $ rest !! 0
--  url      ← pure $ rest !! 1
--  files    ← pure $ map wrap $ tail $ tail rest
--  album    ← (decode' expand) <$> fetch url
--  cmds     ← pure $ (commands genre absolute) <$> album
--  _        ← case cmds of
--    Nothing → pure ()
--    Just α  → runCmds α files
--  return ()
