{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Managed.Connectors.HTTPConnector.Internal
  ( ManagedAPI
  , mkServer
  , mkApp
  ) where

import Control.Exception (displayException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Managed hiding (JSON)
import Data.Managed.Instances.JSON
import Managed.Agent
import Managed.Exception
import Network.Wai
import Servant

type ManagedAPI
   = "probes" :> (Get '[ JSON] [ProbeID] :<|> Capture "probe" ProbeID :> Get '[ JSON] ProbeDescription :<|> Capture "probe" ProbeID :> "invoke" :> ReqBody '[ JSON] [String] :> Post '[ JSON] String)

mkServer :: Agent SR -> Server ManagedAPI
mkServer agent =
  handleList agent :<|> handleDescribe agent :<|> handleInvoke agent

handleList :: Agent SR -> Handler [ProbeID]
handleList = return . ids

handleDescribe :: Agent SR -> [Char] -> Handler ProbeDescription
handleDescribe a p = safely (return $ describeEither a p)

handleInvoke :: Agent SR -> ProbeID -> [String] -> Handler String
handleInvoke agent probe args = safely (invoke agent probe args)

errCode :: AgentException -> ServerError
errCode (ProbeRuntimeException _) = err500
errCode _ = err400

mkApp :: Agent SR -> Application
mkApp agent = serve (Proxy @ManagedAPI) (mkServer agent)

safely :: IO (Either AgentException b) -> Handler b
safely action = do
  x <- liftIO action
  case x of
    Left e ->
      throwError $
      (errCode e) {errBody = Data.Aeson.encode . displayException $ e}
    Right val -> return val
