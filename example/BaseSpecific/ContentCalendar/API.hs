{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -fno-warn-unused-binds #-}
-- |
module BaseSpecific.ContentCalendar.API
  ( listRecords,
  )
where

------------------------------------------------------------------------------

import BaseSpecific.ContentCalendar.Types
import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client (ClientM, client)
import Scully.Utils

------------------------------------------------------------------------------

-- | https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
type ContentCalendarAPI =
  StrictHeader "Authorization" Text :>"Content pipeline" :> QueryParam "maxRecords" Int :> QueryParam "view" Text :> Get '[JSON] CPTable

------------------------------------------------------------------------------
contentCalendarAPI :: Proxy ContentCalendarAPI
contentCalendarAPI = Proxy 

------------------------------------------------------------------------------
listRecords :: Maybe Text -> Maybe Int -> Maybe Text -> ClientM CPTable
listRecords = client contentCalendarAPI

