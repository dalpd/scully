{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Shared types for scully.
module Scully.Types
  ( -- * Top level return types for Airtable Metadata API.
    Bases,
    Tables,

    BaseId,
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Text (Text)

------------------------------------------------------------------------------

type BaseId = Text

newtype Bases = Bases [Base]

newtype Tables = Tables [Table]

data Table = Table
  {
  }

data Base = Base
  {
  }
