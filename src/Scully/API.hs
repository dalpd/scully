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

------------------------------------------------------------------------------

-- | https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
type AirtableMetadataAPI = Text

------------------------------------------------------------------------------
airtableMetadataAPI :: Proxy AirtableMetadataAPI
airtableMetadataAPI = Proxy 

------------------------------------------------------------------------------
