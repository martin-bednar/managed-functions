{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Managed.ProbeDescription
  ( ProbeDescription(..)
  ) where

import Data.Managed.Agent (ProbeID)
import GHC.Generics (Generic)

data ProbeDescription =
  ProbeDescription
    { probeID :: ProbeID
    , probeType :: String
    , probeParams :: [String]
    , probeReturns :: String
    }
  deriving (Show, Eq, Generic)
