module Datasource.DiscogsRepository where

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

--for debug
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson (eitherDecode)
import Datasource.Models.AlbumResponse (AlbumResponse(..))

-- Overloaded strings, no types declared
url = "https://api.discogs.com/releases/"
userAgent = "haskell-discogs"
headers = [("User-Agent", userAgent), ("Accept-Encoding", "gzip")]

makeUrl ∷ String → String
makeUrl = mappend url . last . splitOn "/"

makeRequest ∷ String → IO Request
makeRequest = addHeaders ◁ parseRequest . makeUrl
  where addHeaders α = α { requestHeaders = headers}

fetchFromDiscogs ∷ String → IO ByteString
fetchFromDiscogs α = do
  request  ← makeRequest α
  manager  ← newManager tlsManagerSettings
  response ← httpLbs request manager
  pure $ responseBody response

fetch ∷ String → IO (Either String ByteString)
fetch = annotateErr ◁ try . fetchFromDiscogs
  where annotateErr = left (const errMsg ∷ IOException → String)
        errMsg      = "error fetching from Discogs"

-- debug
debug ∷ String → AlbumResponse
debug α = case (unsafePerformIO (fetch α) >>= eitherDecode) of
  (Right α) → α
  (Left  _) → AlbumResponse [] "" [] Nothing
