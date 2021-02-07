-- | A set of helper functions to interact with Airtable Metadata API.
module Scully.Client
  ( -- *
    listBases,
    listTables
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)
import qualified Scully.API as Scully
import Scully.Types
import Scully.Utils
import Servant.Client (ClientError, runClientM)

------------------------------------------------------------------------------

-- | Convenience function to use `Scully.listBases`.
listBases ::
  Text ->
  Text ->
  IO (Either ClientError Bases)
listBases auth secret = do
  env <- defaultEnv
  runClientM (api (Just auth) (Just secret)) env
  where
    api = Scully.listBases

-- | Convenience function to use `Scully.listTables`.
listTables ::
  Text ->
  Text ->
  BaseId ->
  IO (Either ClientError Tables)
listTables auth secret baseId = do
  env <- defaultEnv
  runClientM (api (Just auth) (Just secret) baseId) env
  where
    api = Scully.listTables
