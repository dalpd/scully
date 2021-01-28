-- | 
module Tablecloth
  ( main
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)

import Tablecloth.Client (listRecords)
import Tablecloth.Utils (accessKey)

------------------------------------------------------------------------------
main :: IO ()
main = do
  key <- accessKey "api_key"
  records <- listRecords (Just $ "Bearer " <> key) Nothing Nothing
  print records
  pure ()

