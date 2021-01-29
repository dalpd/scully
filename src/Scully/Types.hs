{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
-- | Shared types for scully.
module Scully.Types
  ( CPTable,
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON, Value)
import qualified Data.Aeson as A
import Data.Text (Text)

------------------------------------------------------------------------------

-- | Content pipeline table.
newtype CPTable = CPTable [CPRecord]
  deriving newtype Show

-- | `FromJSON` instance of `CPTable`.
instance FromJSON CPTable where
  parseJSON = A.withObject "CPTable" $ \o -> do
    records <- o A..: "records"
    pure $
      CPTable records

------------------------------------------------------------------------------

-- |
data CPRecord = CPRecord
  { _c_p_record_id :: Text,
    _c_p_record_fields :: Value
  }
  deriving stock Show

-- | `FromJSON` instance of `CPRecord`.
instance FromJSON CPRecord where
  parseJSON = A.withObject "CPRecord" $ \o -> do
    cPRecordId <- o A..: "id"
    cPRecordFields <- o A..: "fields"
    pure $
      CPRecord
        { _c_p_record_id = cPRecordId,
          _c_p_record_fields = cPRecordFields
        }
