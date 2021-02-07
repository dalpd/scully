{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module containing type representations for bases.
module Scully.Types.Base
  ( Base,
    Bases,
    BaseId,

    -- * Lenses, Isos and Prisms
    basePermissionLevelText,
  )
where

------------------------------------------------------------------------------

import Control.Lens (Iso', iso, view)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Text (Text)
import Servant.API (ToHttpApiData, toUrlPiece)

------------------------------------------------------------------------------

-- | Newtype wrapper for base identifiers.
newtype BaseId = BaseId Text
  deriving newtype (FromJSON, Show)

-- | >>> toUrlPiece $ BaseId "id"
-- "id"
instance ToHttpApiData BaseId where
  toUrlPiece (BaseId bid) = bid


------------------------------------------------------------------------------

-- | Newtype wrapper for base names.
newtype BaseName = BaseName Text
  deriving newtype (FromJSON, Show)

------------------------------------------------------------------------------

-- | Sum type defining the possible different levels of access.
-- API consumers are expected to handle unknown permission levels
-- gracefully by falling back to read behavior.
data BasePermissionLevel
  = BasePermissionLevel_Read
  | BasePermissionLevel_Comment
  | BasePermissionLevel_Edit
  | BasePermissionLevel_Create

instance Show BasePermissionLevel where
  show level = show $ view basePermissionLevelText level

-- | Iso to go between BasePermissionLevel and Text.
basePermissionLevelText :: Iso' BasePermissionLevel Text
basePermissionLevelText = iso toText fromText
  where
    toText = \case
      BasePermissionLevel_Read -> "read"
      BasePermissionLevel_Comment -> "comment"
      BasePermissionLevel_Edit -> "edit"
      BasePermissionLevel_Create -> "create"
    fromText = \case
      "read" -> BasePermissionLevel_Read
      "comment" -> BasePermissionLevel_Comment
      "edit" -> BasePermissionLevel_Edit
      "create" -> BasePermissionLevel_Create
      _ -> BasePermissionLevel_Read

-- TODO(dalp): Create an iso and use that to get this instance.
instance FromJSON BasePermissionLevel where
  parseJSON = pure . \case
    "read" -> BasePermissionLevel_Read
    "comment" -> BasePermissionLevel_Comment
    "edit" -> BasePermissionLevel_Edit
    "create" -> BasePermissionLevel_Create
    _ -> BasePermissionLevel_Read
    
------------------------------------------------------------------------------

-- |
data Base = Base
  { _base_id :: BaseId,
    _base_name :: BaseName,
    _base_permissionLevel :: BasePermissionLevel
  }
  deriving stock Show

instance FromJSON Base where
  parseJSON = A.withObject "Base" $ \o -> do
    _base_id <- o A..: "id"
    _base_name <- o A..: "name"
    _base_permissionLevel <- o A..: "permissionLevel"
    pure $ Base {..}

------------------------------------------------------------------------------

-- | Newtype wrapper for the response "list bases" endpoint returns.
newtype Bases = Bases [Base]
  deriving Show

instance FromJSON Bases where
  parseJSON = A.withObject "Bases" $ \o -> do
    baseList <- o A..: "bases"
    pure $ Bases baseList
