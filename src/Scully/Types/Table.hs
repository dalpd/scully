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
newtype PrimaryFieldId = PrimaryFieldId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for a table's fields' identifiers.
newtype FieldId =  FieldId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for a table's fields' names.
newtype FieldName = FieldName Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | API consumers are expected to handle unknown field types gracefully but
-- API doesn't specify what value to fall back on, my money is on
-- `FieldType_MultilineText`.
data FieldType
  = FieldType_AutoNumber
  | FieldType_Barcode
  | FieldType_Button
  | FieldType_Checkbox
  | FieldType_Count
  | FieldType_CreatedTime
  | FieldType_Currency
  | FieldType_Date
  | FieldType_DateTime
  | FieldType_Duration
  | FieldType_Email
  | FieldType_Formula
  | FieldType_LastModifiedBy
  | FieldType_LastModifiedTime
  | FieldType_MultilineText
  | FieldType_MultipleAttachments
  | FieldType_MultipleCollaborators
  | FieldType_MultipleLookupValues
  | FieldType_MultipleRecordLinks
  | FieldType_MultipleSelects
  | FieldType_Number
  | FieldType_Percent
  | FieldType_PhoneNumber
  | FieldType_Rating
  | FieldType_RichText
  | FieldType_Rollup
  | FieldType_SingleCollaborator
  | FieldType_SingleLineText
  | FieldType_SingleSelect
  | FieldType_Url

instance FromJSON FieldType where
  parseJSON = pure . \case
    "autoNumber" -> FieldType_AutoNumber
    "barcode" -> FieldType_Barcode
    "button" -> FieldType_Button
    "checkbox" -> FieldType_Checkbox
    "count" -> FieldType_Count
    "createdTime" -> FieldType_CreatedTime
    "currency" -> FieldType_Currency
    "date" -> FieldType_Date
    "dateTime" -> FieldType_DateTime
    "duration" -> FieldType_Duration
    "email" -> FieldType_Email
    "formula" -> FieldType_Formula
    "lastModifiedBy" -> FieldType_LastModifiedBy
    "lastModifiedTime" -> FieldType_LastModifiedTime
    "multilineText" -> FieldType_MultilineText
    "multipleAttachments" -> FieldType_MultipleAttachments
    "multipleCollaborators" -> FieldType_MultipleCollaborators
    "multipleLookupValues" -> FieldType_MultipleLookupValues
    "multipleRecordLinks" -> FieldType_MultipleRecordLinks
    "multipleSelects" -> FieldType_MultipleSelects
    "number" -> FieldType_Number
    "percent" -> FieldType_Percent
    "phoneNumber" -> FieldType_PhoneNumber
    "rating" -> FieldType_Rating
    "richText" -> FieldType_RichText
    "rollup" -> FieldType_Rollup
    "singleCollaborator" -> FieldType_SingleCollaborator
    "singleLineText" -> FieldType_SingleLineText
    "singleSelect" -> FieldType_SingleSelect
    "url" -> FieldType_Url
    _ -> FieldType_MultilineText

------------------------------------------------------------------------------

-- | 
data Field = Field
  { _field_id :: FieldId,
    _field_name :: FieldName,
    _field_type :: FieldType
  }

instance FromJSON Field where
  parseJSON = A.withObject "Field" $ \o -> do
    _field_id <- o A..: "id"
    _field_name <- o A..: "name"
    _field_type <- o A..: "type"
    pure $ Field {..}

-- |
newtype Fields = Fields [Field]

instance FromJSON Fields where
  parseJSON = A.withObject "Fields" $ \o -> do
    fieldList <- o A..: "fields"
    pure $ Fields fieldList

------------------------------------------------------------------------------

-- |
newtype ViewId = ViewId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | API consumers are expected to handle unknown view names gracefully but
-- API doesn't specify what value to fall back on, my money is on
-- `ViewName_Grid`.
data ViewName
  = ViewName_Calendar
  | ViewName_Form
  | ViewName_Gallery
  | ViewName_Grid
  | ViewName_Kanban

instance FromJSON ViewName where
  parseJSON = pure . \case
    "Calendar view" -> ViewName_Calendar
    "Form view" -> ViewName_Form
    "Gallery view" -> ViewName_Gallery
    "Grid view" -> ViewName_Grid
    "Kanban view" -> ViewName_Kanban
    _ -> ViewName_Grid

------------------------------------------------------------------------------

-- | API consumers are expected to handle unknown view types gracefully but
-- API doesn't specify what value to fall back on, my money is on
-- `ViewType_Grid`.
data ViewType
  = ViewType_Calendar
  | ViewType_Form
  | ViewType_Gallery
  | ViewType_Grid
  | ViewType_Kanban

instance FromJSON ViewType where
  parseJSON = pure . \case
    "calendar" -> ViewType_Calendar
    "form" -> ViewType_Form
    "gallery" -> ViewType_Gallery
    "grid" -> ViewType_Grid
    "kanban" -> ViewType_Kanban
    _ -> ViewType_Grid

------------------------------------------------------------------------------

-- |
data View = View
  { _view_id :: ViewId,
    _view_name :: ViewName,
    _view_type :: ViewType
  }

instance FromJSON View where
  parseJSON = A.withObject "View" $ \o -> do
    _view_id <- o A..: "id"
    _view_name <- o A..: "name"
    _view_type <- o A..: "type"
    pure $ View {..}

-- |
newtype Views = Views [View]

instance FromJSON Views where
  parseJSON = A.withObject "Views" $ \o -> do
    viewList <- o A..: "views"
    pure $ Views viewList

------------------------------------------------------------------------------

-- |
data Table = Table
  { _table_id :: TableId,
    _table_name :: TableName,
    _table_primaryFieldId :: PrimaryFieldId,
    _table_fields :: Fields,
    _table_views :: Views
  }

instance FromJSON Table where
  parseJSON = A.withObject "Table" $ \o -> do
    _table_id <- o A..: "id"
    _table_name <- o A..: "name"
    _table_primaryFieldId <- o A..: "primaryFieldId"
    _table_fields <- o A..: "fields"
    _table_views <- o A..: "views"
    pure $ Table {..}

-- | Newtype wrapper for the response "list tables" endpoint returns.
newtype Tables = Tables [Table]

instance FromJSON Tables where
  parseJSON = A.withObject "Tables" $ \o -> do
    tableList <- o A..: "tables"
    pure $ Tables tableList
