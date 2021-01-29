-- | 
module Scully
  ( main
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)

import Scully.Client (listRecords)
import Scully.Utils (accessKey)

------------------------------------------------------------------------------
main :: IO ()
main = do
  key <- accessKey "api_key"
  records <- listRecords (Just $ "Bearer " <> key) Nothing Nothing
  print records
  pure ()

