module Fetching where

import Data.List (last)
import Data.List.Split (splitOn)
import System.Process (readProcess)

releaseCode ∷ String → String
releaseCode = last . splitOn "/"

url ∷ String → String
url = mappend "https://api.discogs.com/releases/" . releaseCode

fetch ∷ String → IO String
fetch α = readProcess "curl" [url α, "--user-agent", "haskell-discogs"] ""
