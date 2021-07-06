module Datasource.DiscogsRepository (fetch) where

import Prelude (Either, IO, String, (.), ($), const, pure)
import Control.Arrow (left)
import Control.Exception (Exception, try)
import Data.ByteString.Lazy (ByteString)
import Data.List (last)
import Data.List.Split (splitOn)
import GHC.IO.Exception (IOException)
import Network.HTTP.Conduit (Request, httpLbs, newManager, parseRequest, 
                             requestHeaders, responseBody, tlsManagerSettings)
import System.Process (readProcess)
import Helpers ((◁), (◇), putStderr)

-- Overloaded strings, no types declared
url = "https://api.discogs.com/releases/"
userAgent = "haskell-discogs"
headers = [("User-Agent", userAgent), ("Accept-Encoding", "gzip")]

makeUrl ∷ String → String
makeUrl = (url ◇) . last . splitOn "/"

makeRequest ∷ String → IO Request
makeRequest = addHeaders ◁ parseRequest . makeUrl
  where addHeaders α = α { requestHeaders = headers}

fetchFromDiscogs ∷ String → IO ByteString
fetchFromDiscogs α = do
  request  ← makeRequest α
  manager  ← newManager tlsManagerSettings
  _        ← putStderr "Fetching from Discogs"
  response ← httpLbs request manager
  pure $ responseBody response

fetch ∷ String → IO (Either String ByteString)
fetch = annotateErr ◁ try . fetchFromDiscogs
  where annotateErr = left (const errMsg ∷ IOException → String)
        errMsg      = "Error fetching from Discogs"
