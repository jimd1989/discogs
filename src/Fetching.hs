module Fetching where

import Control.Arrow (left)
import Control.Exception (Exception, try)
import Data.ByteString.Lazy (ByteString)
import Data.List (last)
import Data.List.Split (splitOn)
import GHC.IO.Exception (IOException)
import Network.HTTP.Conduit (Request, httpLbs, newManager, parseRequest, 
                             requestHeaders, responseBody, tlsManagerSettings)
import System.Process (readProcess)
import Helpers ((◁))

-- Overloaded strings, no types declared
url = "https://api.discogs.com/releases/"
userAgent = "haskell-discogs"
headers = [("User-Agent", userAgent), ("Accept-Encoding", "gzip")]

makeUrl ∷ String → String
makeUrl = mappend url . last . splitOn "/"

makeRequest ∷ String → IO Request
makeRequest = fmap addHeaders . parseRequest . makeUrl
  where addHeaders α = α { requestHeaders = headers}

fetch ∷ String → IO ByteString
fetch α = do
  request  ← makeRequest α
  manager  ← newManager tlsManagerSettings
  response ← httpLbs request manager
  pure $ responseBody response

-- This will eventually be the external function of this module 
fetch' ∷ String → IO (Either String ByteString)
fetch' = annotateErr ◁ try . fetch
  where annotateErr = left (const errMsg ∷ IOException → String)
        errMsg      = "error fetching from Discogs"
