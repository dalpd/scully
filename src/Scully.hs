-- | A basic executable for scully to test things out.
module Scully
  ( main
  )
where

------------------------------------------------------------------------------

import Scully.Client (listRecords)
import Scully.Utils (accessKey)

------------------------------------------------------------------------------

-- | Entry point of the scully executable.
main :: IO ()
main = do
  key <- accessKey "api_key"
  records <- listRecords (Just $ "Bearer " <> key) Nothing Nothing
  print records
