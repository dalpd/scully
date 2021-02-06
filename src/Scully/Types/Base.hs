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

import Control.Lens (Iso', iso)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Text (Text)

------------------------------------------------------------------------------

-- | Newtype wrapper for base identifiers.
newtype BaseId = BaseId Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Newtype wrapper for base names.
newtype BaseName = BaseName Text
  deriving newtype FromJSON

------------------------------------------------------------------------------

-- | Sum type defining the possible different levels of access.
data BasePermissionLevel
  = BasePermissionLevel_Read
  | BasePermissionLevel_Comment
  | BasePermissionLevel_Edit
  | BasePermissionLevel_Create
  | BasePermissionLevel_Unknown

-- | Iso to go between BasePermissionLevel and Text.
basePermissionLevelText :: Iso' BasePermissionLevel Text
basePermissionLevelText = iso toText fromText
  where
    toText = \case
      BasePermissionLevel_Read -> "read"
      BasePermissionLevel_Comment -> "comment"
      BasePermissionLevel_Edit -> "edit"
      BasePermissionLevel_Create -> "create"
      BasePermissionLevel_Unknown -> "read"
      -- ^ API consumers are expected to handle unknown permission levels
      -- gracefully by falling back to read behavior.
    fromText = \case
      "read" -> BasePermissionLevel_Read
      "comment" -> BasePermissionLevel_Comment
      "edit" -> BasePermissionLevel_Edit
      "create" -> BasePermissionLevel_Create
      _ -> BasePermissionLevel_Read
      -- ^ API consumers are expected to handle unknown permission levels
      -- gracefully by falling back to read behavior.

-- TODO(dalp): Use the iso to get this instance for free.
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

instance FromJSON Base where
  parseJSON = A.withObject "Base" $ \o -> do
    id' <- o A..: "id"
    name <- o A..: "name"
    permissionLevel <- o A..: "permissionLevel"
    pure $
      Base id' name permissionLevel

------------------------------------------------------------------------------

-- | Newtype wrapper for the response "list bases" endpoint returns.
newtype Bases = Bases [Base]

instance FromJSON Bases where
  parseJSON = A.withObject "Bases" $ \o -> do
    bases <- o A..: "bases"
    pure $
      Bases bases
