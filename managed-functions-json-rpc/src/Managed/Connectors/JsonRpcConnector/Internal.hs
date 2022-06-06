{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Managed.Connectors.JsonRpcConnector.Internal where

import Data.Aeson
import Data.Managed (Agent)
import Data.Managed.Encodings.JSON
import Data.Text (Text)
import GHC.Generics (Generic)
import Managed.Agent (invoke)
import Managed.Exception (AgentException)

data Request
  = SimpleRequest ID RqContents
  | Notification RqContents
  | RqBatch [Request]
  deriving (Eq, Show, Generic)

instance FromJSON Request where
  parseJSON =
    withObject "Request" $ \v -> do
      (_ :: String) <- v .: "jsonrpc"
      contents <-
        RqContents <$> v .: "method" <*> v .: "params"
      rId <- v .: "id"
      return $ SimpleRequest rId contents

type ID = Integer

data RqContents =
  RqContents
    { method :: String
    , params :: [Value]
    }
  deriving (Eq, Show, Generic)

data Response
  = Result RsContents
  | Error ErContents
  | RsBatch [Response]
  deriving (Eq, Show, Generic)

instance ToJSON Response where
  toJSON (Result contents) =
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= rsId contents
      , "result" .= rsVal contents
      ]

data RsContents =
  RsContents
    { rsId :: ID
    , rsVal :: Value
    }
  deriving (Eq, Show, Generic)

data ErContents =
  ErContents
    { erCode :: Integer
    , erMsg :: String
    , erId :: Maybe ID
    }
  deriving (Eq, Show, Generic)

handleWith :: Agent JSON -> Request -> IO Response
handleWith a rq =
  case rq of
    SimpleRequest rId rCon ->
      mkRsp rId <$> invoke a (method rCon) (params rCon)

mkRsp :: ID -> Either AgentException Value -> Response
mkRsp rId result =
  case result of
    Right val -> Result $ RsContents rId val
