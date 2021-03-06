{-# LANGUAGE DataKinds #-}
-- | A module for utilities used/provided by scully.
module Scully.Utils
  ( accessKey,

    -- ^ Servant API utilities
    RequiredQueryParam,
    StrictHeader,
    GetJSON,

    -- ^ Servant Client utilities
    defaultEnv,
    mkBaseUrl,
  )
where 

------------------------------------------------------------------------------

import Data.Text
import LoadEnv
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (Https), mkClientEnv)
import System.Environment (lookupEnv)

------------------------------------------------------------------------------


-- | Type synonym for required query params.
type RequiredQueryParam = QueryParam' '[Required]

-- | Type synonym for strict headers.
type StrictHeader = Header' '[Strict] 

-- | Type synonym for GET operations returning JSON.
type GetJSON returnType = Get '[JSON] returnType

------------------------------------------------------------------------------

-- | The default `ClientEnv` for Airtable (Metadata) API.
defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager tlsManagerSettings
  pure $ mkClientEnv manager $ mkBaseUrl version
  where    
    version = "v0"

------------------------------------------------------------------------------

-- | Construct a `BaseUrl` for Airtable (Metadata) API given an API version
-- and a base id.
mkBaseUrl :: Text -> BaseUrl
mkBaseUrl version =
  BaseUrl Https baseUrlHost baseUrlPort baseUrlPath
  where
    baseUrlHost = "api.airtable.com"
    baseUrlPort = 443
    baseUrlPath = unpack version <> "/meta"

------------------------------------------------------------------------------

-- | A utility function to lookup a certain key in the .env file in the
-- directory you're calling this function from. If you get a hit `accessKey`
-- will return the associated value and if not, an error.
accessKey :: String -> IO Text
accessKey key = loadEnv >> lookupEnv key >>= \case
  Just k -> pure $ pack k
  Nothing -> error $ errorMsg key
  where
    errorMsg k = "Couldn't find `" <> k <> "` in .env"
