module Fetching where

import Data.List (last)
import Data.List.Split (splitOn)
import System.Process (readProcess)

releaseCode ∷ [Char] → [Char]
releaseCode = last . splitOn "/"

url ∷ [Char] → [Char]
url = mappend "https://api.discogs.com/releases/" . releaseCode

fetch ∷ [Char] → IO [Char]
fetch α = readProcess "curl" [url α, "--user-agent", "haskell-discogs"] ""
