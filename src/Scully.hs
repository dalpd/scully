-- | A basic executable for scully to test things out.
module Scully
  ( main
  )
where

------------------------------------------------------------------------------

import Scully.Utils (accessKey)
import Scully.Client (listBases)

------------------------------------------------------------------------------

-- | Entry point of the scully executable.
main :: IO ()
main = do
  apiKey <- accessKey "api_key"
  userAPIKey <- accessKey "user_api_key"
  listBases apiKey userAPIKey >>= \case
    Left clientError -> print clientError
    Right bases -> print bases
