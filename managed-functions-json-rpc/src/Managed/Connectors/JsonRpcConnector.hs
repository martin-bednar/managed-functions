{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Managed.Connectors.JsonRpcConnector where

import Control.Monad.IO.Class (liftIO)
import Data.Managed (Agent, Connector(..))
import Data.Managed.Encodings.JSON
import Managed.Connectors.JsonRpcConnector.Internal
import qualified Network.Wai.Handler.Warp as Warp
import Servant

type JsonRpcApi
   = ReqBody '[ Servant.JSON] Request :> Post '[ Servant.JSON] Response

jsonRpcConnector :: Connector Data.Managed.Encodings.JSON.JSON
jsonRpcConnector = Connector {run = Warp.run 3000 . mkApp}

mkApp :: Agent Data.Managed.Encodings.JSON.JSON -> Application
mkApp agent = serve (Proxy @JsonRpcApi) $ mkServer agent

mkServer :: Agent Data.Managed.Encodings.JSON.JSON -> Server JsonRpcApi
mkServer a r = hdl
  where
    hdl :: Handler Response
    hdl = liftIO $ handleWith a r
