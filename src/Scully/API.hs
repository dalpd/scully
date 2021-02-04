{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -fno-warn-unused-binds #-}
-- |
module Scully.API
  (
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Scully.Utils
import Scully.Types
import Servant.API
import Servant.Client (ClientM, client)

------------------------------------------------------------------------------


-- | Type synonym for an "Authorization" header.
-- Usage:
-- Authorization: Bearer $USER_API_KEY
type AuthHeader = Header "Authorization" Text

-- | Type synonym for the "X-Airtable-Client-Secret" header.
-- Usage:
-- X-Airtable-Client-Secret: $YOURCLIENTSECRET
type AirtableSecret = Header "X-Airtable-Client-Secret" Text

-- | Type synonym for the two headers needed by Airtable Metadata API.
type Authorization = AuthHeader :> AirtableSecret

-- | Airtable Metadata API representation
type AirtableMetadataAPI = ListBases :<|> ListTables

------------------------------------------------------------------------------

-- | List bases:
-- GET <https://api.airtable.com/v0/meta/bases>
-- Returns the list of bases the API key can access in the order they appear
-- on the user's home screen. The result will be truncated to only include
-- the first 1000 bases.
type ListBases = "bases" :> Authorization :> GetJSON Bases

-- | List tables:
-- GET <https://api.airtable.com/v0/meta/bases/BaseId/tables>
-- Returns the schema of the tables in the specified base.
type ListTables =  "bases" :> Authorization :> BaseId :> "tables" :> GetJSON Tables

------------------------------------------------------------------------------
airtableMetadataAPI :: Proxy AirtableMetadataAPI
airtableMetadataAPI = Proxy 

------------------------------------------------------------------------------
