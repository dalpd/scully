{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module containing type representations for tables.
module Scully.Types.Table
  ( Table,
    Tables,
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Text (Text)

------------------------------------------------------------------------------

-- | Newtype wrapper for table identifiers.
newtype TableId = TableId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for table names.
newtype TableName = TableName Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for a table's primary field identifier.
newtype TablePrimaryFieldId = TablePrimaryFieldId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for a table's fields' identifiers.
newtype TableFieldId =  TableFieldId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for a table's fields' names.
newtype TableFieldName = TableFieldName Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- |
data TableFieldType
  = TableFieldType_AutoNumber
  | TableFieldType_Unknown

-- TODO(dalp): Create an iso and use that to get this instance for free.
instance FromJSON TableFieldType where
  parseJSON = pure . \case
    "autoNumber" -> TableFieldType_AutoNumber
    _ -> TableFieldType_Unknown
    
-- autoNumber
-- barcode
-- button
-- checkbox
-- count
-- createdBy
-- createdTime
-- currency
-- date
-- dateTime
-- duration
-- email
-- formula
-- lastModifiedBy
-- lastModifiedTime
-- multilineText
-- multipleAttachments
-- multipleCollaborators
-- multipleLookupValues
-- multipleRecordLinks
-- multipleSelects
-- number
-- percent
-- phoneNumber
-- rating
-- richText
-- rollup
-- singleCollaborator
-- singleLineText
-- singleSelect
-- url
 
------------------------------------------------------------------------------

-- | 
data TableField = TableField
  { _tableField_id :: TableFieldId,
    _tableField_name :: TableFieldName,
    _tableField_type :: TableFieldType
  }

instance FromJSON TableField where
  parseJSON = A.withObject "TableField" $ \o -> do
    id' <- o A..: "id"
    name <- o A..: "name"
    type' <- o A..: "type"
    pure $
      TableField id' name type'

------------------------------------------------------------------------------

-- |
newtype TableFields = TableFields [TableField]

instance FromJSON TableFields where
  parseJSON = A.withObject "TableFields" $ \o -> do
    fields <- o A..: "fields"
    pure $ TableFields fields

------------------------------------------------------------------------------

-- |
data Table = Table
  { _table_id :: TableId,
    _table_name :: TableName,
    _table_primaryFieldId :: TablePrimaryFieldId,
    _table_fields :: TableFields
  }

instance FromJSON Table where
  parseJSON = A.withObject "Table" $ \o -> do
    id' <- o A..: "id"
    name <- o A..: "name"
    primaryFieldId <- o A..: "primaryFieldId"
    fields <- o A..: "fields"
    pure $
      Table id' name primaryFieldId fields

------------------------------------------------------------------------------

-- | Newtype wrapper for the response "list tables" endpoint returns.
newtype Tables = Tables [Table]

instance FromJSON Tables where
  parseJSON = A.withObject "Tables" $ \o -> do
    tables <- o A..: "tables"
    pure $ Tables tables
