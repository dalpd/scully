-- | A set of helper functions to interact with Airtable "Content Calendar"
-- API.
module BaseSpecific.ContentCalendar.Client
  ( listRecords,
  )
where

------------------------------------------------------------------------------

import qualified BaseSpecific.ContentCalendar.API as API
import BaseSpecific.ContentCalendar.Types (CPTable)
import Data.Text (Text)
import Scully.Utils (defaultEnv)
import Servant.Client (ClientError, runClientM)

------------------------------------------------------------------------------

-- | Helper function for searching photos.
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

