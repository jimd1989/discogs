module Datasource.DiscogsRepository (fetch) where

import Prelude (Either, IO, String, (.), ($), pure)
import Control.Error.Util (note)
import Control.Monad.Except (ExceptT, lift, liftEither)
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)
import Network.HTTP.Conduit (Request, httpLbs, newManager, parseRequest, 
                             requestHeaders, responseBody, tlsManagerSettings)
import Helpers ((◁), (◀), (◇), head', last', putStderr)

-- Overloaded strings, no types declared
url = "https://api.discogs.com/releases/"
userAgent = "haskell-discogs"
headers = [("User-Agent", userAgent), ("Accept-Encoding", "gzip")]

makeUrl ∷ String → Either String String
makeUrl = (url ◇) ◁ err . (head' . splitOn "-" ◀ last' . splitOn "/")
  where err = note "Error parsing URL"

makeRequest ∷ String → Either String Request
makeRequest = addHeaders ◁ parse ◀ makeUrl
  where parse        = note "Error parsing request" . parseRequest
        addHeaders α = α { requestHeaders = headers }

fetch ∷ String → ExceptT String IO ByteString
fetch α = do
 request  ← liftEither $ makeRequest α
 manager  ← lift       $ newManager tlsManagerSettings
 _        ← lift       $ putStderr "Fetching from Discogs"
 response ← lift       $ httpLbs request manager
 pure $ responseBody response
