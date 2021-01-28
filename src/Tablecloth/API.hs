{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |
module Tablecloth.API
  ( listRecords,
  )
where

------------------------------------------------------------------------------

import Tablecloth.Types

import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client (ClientM, client)

------------------------------------------------------------------------------

-- | Type synonym for required query params.
type RequiredQueryParam = QueryParam' '[Required]

type StrictHeader = Header' '[Strict] 

------------------------------------------------------------------------------

-- | TODO(dalp): Document and refactor.
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
type ContentCalendarAPI =
  StrictHeader "Authorization" Text :>"Content pipeline" :> QueryParam "maxRecords" Int :> QueryParam "view" Text :> Get '[JSON] CPTable

------------------------------------------------------------------------------
contentCalendarAPI :: Proxy ContentCalendarAPI
contentCalendarAPI = Proxy 

------------------------------------------------------------------------------
listRecords :: Maybe Text -> Maybe Int -> Maybe Text -> ClientM CPTable
listRecords = client contentCalendarAPI

