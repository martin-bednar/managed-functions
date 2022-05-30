{-# LANGUAGE ScopedTypeVariables #-}

module Managed.Probe
  ( params
  , returns
  ) where

import Data.Managed (Probe(..), typeRep)
import Data.Typeable (TypeRep)
import qualified Managed.Probe.Internal.Params as P

params :: Probe e -> [TypeRep]
params = P.params . typeRep

returns :: Probe e -> TypeRep
returns = P.returns . typeRep
