{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Managed.Encodings.JSON where

import Data.Aeson (FromJSON, Result(..), ToJSON, Value, fromJSON, toJSON)
import Data.Managed.Encoding

data JSON

instance Encoding JSON where
  type In JSON = Value
  type Out JSON = Value

instance (ToJSON a) => Encode a JSON where
  encode = toJSON

instance (FromJSON a) => Decode a JSON where
  decode v =
    case fromJSON v of
      Error _ -> Nothing
      Success x -> x
