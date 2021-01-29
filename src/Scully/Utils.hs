-- | A module for utilities used/provided by scully.
module Scully.Utils
  ( accessKey
  )
where 

------------------------------------------------------------------------------

import Data.Text
import LoadEnv
import System.Environment (lookupEnv)

------------------------------------------------------------------------------

-- | A utility function to lookup a certain key in the .env file in the
-- directory you're calling this function from. If you get a hit `accessKey`
-- will return the associated value and if not, an error.
accessKey :: String -> IO Text
accessKey key = loadEnv >> lookupEnv key >>= \case
  Just k -> pure $ pack k
  Nothing -> error $ errorMsg key
  where
    errorMsg k = "Couldn't find `" <> k <> "` in .env"
