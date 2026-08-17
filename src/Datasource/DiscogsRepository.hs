module Datasource.DiscogsRepository (fetch) where

import Prelude (String, (.), ($), pure)
import Control.Error.Util (note)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)
import Network.HTTP.Conduit (Request, httpLbs, newManager, parseRequest, 
                             requestHeaders, responseBody, tlsManagerSettings)
import Helpers ((◁), (◀), (◇), head', last', note', putStderr)

url = "https://api.discogs.com/releases/"
userAgent = "haskell-discogs"
headers = [("User-Agent", userAgent), ("Accept-Encoding", "gzip")]

makeUrl ∷ MonadError String m ⇒ String → m String
makeUrl = (url ◇) ◁ err . (head' . splitOn "-" ◀ last' . splitOn "/")
  where err = note' "Error parsing URL"

makeRequest ∷ MonadError String m ⇒ String → m Request
makeRequest = addHeaders ◁ parse ◀ makeUrl
  where parse        = note' "Error parsing request" . parseRequest
        addHeaders α = α { requestHeaders = headers }

fetch ∷ (MonadError String m, MonadIO m) ⇒ String → m ByteString
fetch α = do
 request  ← makeRequest α
 manager  ← liftIO $ newManager tlsManagerSettings
 _        ← putStderr "Fetching from Discogs"
 response ← liftIO $ httpLbs request manager
 pure $ responseBody response
