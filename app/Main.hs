module Main where

import System.Environment (getArgs)
import Fetching (fetch)
import Parsing (decode')
import Processing (commands)

release ∷ [Char]
--release = "https://www.discogs.com/Akiko-Yano-%E3%81%9F%E3%81%A0%E3%81%84%E3%81%BE/release/12652701"
release = "https://www.discogs.com/%E7%9F%A2%E9%87%8E%E9%A1%95%E5%AD%90-%E3%81%9F%E3%81%A0%E3%81%84%E3%81%BE/release/1852193"

main :: IO ()
main = do
  album ← decode' <$> fetch release
  cmds  ← pure $ (commands "Pop") <$> album
  _ ← case cmds of
    Nothing → pure ()
    Just α  → mapM_ putStrLn α
  return ()
