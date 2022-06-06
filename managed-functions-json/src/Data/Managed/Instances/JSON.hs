module Data.Managed.Instances.JSON where

import Data.Aeson (FromJSON, ToJSON)
import Managed

instance ToJSON ProbeDescription

instance FromJSON ProbeDescription

instance ToJSON AgentException

instance FromJSON AgentException
