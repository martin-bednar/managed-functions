module Data.Managed.Connector where

import Data.Managed.Agent

newtype Connector e =
  Connector
    { run :: Agent e -> IO ()
    }
