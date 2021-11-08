module Datasource.DiscogsRepository (fetch) where

import Prelude (Either, IO, String, (.), ($), const, pure)
import Control.Arrow (left)
import Control.Error.Util (note)
import Control.Monad.Except (ExceptT(..), lift, liftEither, runExceptT)
import Data.ByteString.Lazy (ByteString)
import Data.List (last)
import Data.List.Split (splitOn)
import Data.Traversable (sequence, traverse)
import Network.HTTP.Conduit (Request, httpLbs, newManager, parseRequest, 
                             requestHeaders, responseBody, tlsManagerSettings)
import System.Process (readProcess)
import Helpers ((◁), (◀), (◇), head', last', putStderr)

-- Overloaded strings, no types declared
url = "https://api.discogs.com/releases/"
userAgent = "haskell-discogs"
headers = [("User-Agent", userAgent), ("Accept-Encoding", "gzip")]

makeUrl ∷ String → Either String String
makeUrl = (url ◇) ◁ err . (head' . splitOn "-" ◀ last' . splitOn "/")
  where err = note "Error parsing URL"

makeRequest ∷ String → IO (Either String Request)
makeRequest = traverse (addHeaders ◁ parseRequest) . makeUrl
  where addHeaders α = α { requestHeaders = headers}

fetch ∷ String → IO (Either String ByteString)
fetch α = runExceptT $ do
 request  ← ExceptT $ makeRequest α
 manager  ← lift    $ newManager tlsManagerSettings
 _        ← lift    $ putStderr "Fetching from Discogs"
 response ← lift    $ httpLbs request manager
 pure $ responseBody response
