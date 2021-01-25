-- | 
module Tablecloth
  ( main
  )
where

------------------------------------------------------------------------------

import Tablecloth.Utils (accessKey)

------------------------------------------------------------------------------
main :: IO ()
main = do
  _key <- accessKey "api_key"
  -- something <- Client.getSomething
  pure ()

