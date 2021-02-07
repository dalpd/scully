{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -fno-warn-unused-binds #-}
-- |
module Scully.API
  ( -- *
    listBases,
    listTables,
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


-- | Type synonym for "Authorization" header.
-- Usage:
-- Authorization: Bearer $USER_API_KEY
type Authorization = Header "Authorization" Text

-- | Type synonym for "X-Airtable-Client-Secret" header.
-- Usage:
-- X-Airtable-Client-Secret: $YOURCLIENTSECRET
type AirtableClientSecret = Header "X-Airtable-Client-Secret" Text

-- | Airtable Metadata API representation
type AirtableMetadataAPI
  = "bases" :> (ListBases :<|> ListTables)

------------------------------------------------------------------------------

-- | List bases:
--
-- GET <https://api.airtable.com/v0/meta/bases>
--
-- Returns the list of bases the API key can access in the order they appear
-- on the user's home screen. The result will be truncated to only include
-- the first 1000 bases.
type ListBases = Authorization :> AirtableClientSecret :> GetJSON Bases

-- | List tables:
--
-- GET <https://api.airtable.com/v0/meta/bases/BaseId/tables>
--
-- Returns the schema of the tables in the specified base.
type ListTables = Authorization :> AirtableClientSecret :> Capture "BaseId" BaseId :> "tables" :> GetJSON Tables

------------------------------------------------------------------------------

-- |
airtableMetadataAPI :: Proxy AirtableMetadataAPI
airtableMetadataAPI = Proxy 

------------------------------------------------------------------------------

-- |
listBases ::
  Maybe Text ->
  Maybe Text ->
  ClientM Bases

-- |
listTables ::
  Maybe Text ->
  Maybe Text ->
  BaseId ->
  ClientM Tables

(listBases :<|> listTables) = client airtableMetadataAPI
