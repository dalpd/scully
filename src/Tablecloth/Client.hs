-- | A set of helper functions to interact with Airtable API.
module Tablecloth.Client
  ( listRecords,
  )
where

------------------------------------------------------------------------------

import Data.Text (Text, unpack)

import  Network.HTTP.Client.TLS (tlsManagerSettings)
import  Network.HTTP.Client (newManager)
import  Servant.Client (BaseUrl (..), ClientError, ClientEnv,
                                 Scheme (Https), mkClientEnv, runClientM)

import qualified Tablecloth.API as API
import Tablecloth.Utils (accessKey)
import Tablecloth.Types (CPTable)

------------------------------------------------------------------------------

-- | Helper function for searching photos
listRecords ::
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  IO (Either ClientError CPTable)
listRecords authHeader maxRecords view = do
  env <- defaultEnv
  runClientM (api authHeader maxRecords view) env
  where
    api = API.listRecords

------------------------------------------------------------------------------

defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager tlsManagerSettings
  baseId <- accessKey "base_id"
  pure $ mkClientEnv manager $ mkBaseUrl version baseId
  where    
    version = "v0"

------------------------------------------------------------------------------

-- |
mkBaseUrl :: Text -> Text -> BaseUrl
mkBaseUrl version baseId =
  BaseUrl Https baseUrlHost baseUrlPort baseUrlPath
  where
    baseUrlHost = "api.airtable.com"
    baseUrlPort = 443
    baseUrlPath = unpack version <> "/" <> unpack baseId
