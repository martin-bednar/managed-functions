{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Managed.Encodings.ShowRead where

import Data.Managed.Encoding
import Text.Read (readMaybe)

data SR

instance Encoding SR where
  type In SR = String
  type Out SR = String

instance (Show a) => Encode a SR where
  encode = show

instance (Read a) => Decode a SR where
  decode = readMaybe
