{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
-- |
module Tablecloth.Types
  ( CPTable
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Text (Text)

------------------------------------------------------------------------------

-- | Content pipeline table.
newtype CPTable = CPTable [CPRecord]
  deriving newtype Show

instance FromJSON CPTable where
  parseJSON = A.withObject "Table" $ \o -> do
    records <- o A..: "records"
    pure $
      CPTable records

------------------------------------------------------------------------------

-- |
newtype CPRecord = CPRecord Text
  deriving newtype Show

instance FromJSON CPRecord where
  parseJSON = A.withObject "CPRecord" $ \o -> do
    fields <- o A..: "records"
    pure $
      CPRecord fields
