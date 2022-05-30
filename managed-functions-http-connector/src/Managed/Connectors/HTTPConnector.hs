module Managed.Connectors.HTTPConnector
  ( httpConnector
  , HTTPConnectorAPI
  , httpConnectorServer
  ) where

import Data.Managed (Agent, Connector(..))
import Data.Managed.Encodings.ShowRead
import Managed.Connectors.HTTPConnector.Internal
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Server)

httpConnector :: Connector SR
httpConnector = Connector {run = Warp.run 3000 . mkApp}

type HTTPConnectorAPI = ManagedAPI

httpConnectorServer :: Agent SR -> Server ManagedAPI
httpConnectorServer = mkServer
